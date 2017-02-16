
## x and y are vectors of length 2
angle <- function(x, y) {
    atan2(y[2] - y[1], x[2] - x[1])
}

## extend direction from pt 1 to pt 2
extend <- function(x, y, len) {
    a <- angle(x[2:1], y[2:1])
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
    perp(x, y, len, (angle(x[1:2], y[1:2]) + angle(x[2:3], y[2:3]))/2, 2)
}

################################################################################
## Functions for rendering vw* object outlines

vwlinePolygon <- function() {
    function(x, y, id.lengths, gp, name) {
        polygonGrob(x, y, default.units="in",
                    id.lengths=id.lengths, gp=gp, name=name)
    }
}

vwlinePath <- function(rule="winding") {
    function(x, y, id.lengths, gp, name) {
        pathGrob(x, y, default.units="in",
                 id.lengths=id.lengths, rule=rule, gp=gp, name=name)
    }
}
