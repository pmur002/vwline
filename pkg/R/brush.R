
## Place a brush at a location, size, and orientation
## Brush here is polyline or polygon defined in [-1, 1] coordinates
## List of x and y (as per 'polyclip' package)
placeBrush <- function(brush,
                       x, y, 
                       size=1,
                       angle=0) {
    trans <- transformation(list(translation(x, y),
                                 rotation(angle),
                                 scaling(size/2, size/2)))
    transform(brush, trans)
}

## Angle at start/end vertex is angle of first/last segment
brushEndAngle <- function(x, y) {
    angle(x, y)
}

## Angle at other vertex is (circular) average of angle of previous/next segment
brushAngle <- function(x, y) {
    avgangle(x, y)
}

################################################################################
## Sweeping brushes

## Make a segment of a path by calculating the convex hull of a brush
## located at either end of the segment
makeSegment <- function(brush1, brush2, debug=FALSE) {
    allpts <- list(x=c(brush1$x, brush2$x),
                   y=c(brush1$y, brush2$y))
    hull <- chull(allpts)
    if (debug) {
        grid.polygon(allpts$x[hull], allpts$y[hull],
                     default.units="in", gp=gpar(col=NA, fill=rgb(1,0,0,.5)))
    }
    list(x=allpts$x[hull], y=allpts$y[hull])
}

## Make a corner of a path by calculating the convex hull of the
## brush rotated aboutsize1 corner vertex
makeCorner <- function(brush, x, y, size, debug=FALSE) {
    angle1 <- angle(c(x[1], x[2]), c(y[1], y[2]))
    angle2 <- angle(c(x[2], x[3]), c(y[2], y[3]))
    ## Calculate number of rotations so that the furthermost vertex
    ## of the brush only moves at most .1in on each rotation
    dangle <- angle2 - angle1
    if (size > 0 && abs(dangle) > 0) {
        arclength <- abs(dangle)/2*pi*size
        numRots <- ceiling(arclength/.1)
        rotAngle <- dangle/numRots
        cornerPoly <- placeBrush(brush, x[2], y[2], size, angle1)
        for (i in 1:numRots) {
            rotBrush <- placeBrush(brush, x[2], y[2], size, angle1 + rotAngle*i)
            if (!all(is.finite(c(cornerPoly$x, rotBrush$x, cornerPoly$y, rotBrush$y)))) browser() 
            newPoly <- list(x=c(cornerPoly$x, rotBrush$x),
                            y=c(cornerPoly$y, rotBrush$y))
            hull <- chull(newPoly)
            cornerPoly <- list(x=newPoly$x[hull], y=newPoly$y[hull])
        }
        if (debug) {
            grid.polygon(cornerPoly$x, cornerPoly$y,
                         default.units="in",
                         gp=gpar(col=NA, fill=rgb(0,1,0,.5)))
        }
        cornerPoly
    } else {
        result <- placeBrush(brush, x[2], y[2], size, angle1)
        if (debug) {
            grid.polygon(result$x, result$y,
                         default.units="in",
                         gp=gpar(col=NA, fill=rgb(0,0,1,.5)))
        }
        result
    }
}

################################################################################
## Combining brushes, or the result of sweeping brushes

combineShapes <- function(x, y) {
    ## Handle when either x or y is "empty" polygon
    if (length(x) && length(y)) {
        polyclip(x, y, op="union")
    } else {
        if (length(x)) {
            x
        } else if (length(y)) {
            y
        } else {
            list()
        }
    }
}

################################################################################
## Some predefined brushes

## Important that brush is not zero-extent
## (otherwise, union of segments can fail to join neighbouring segments)
## NOTE that units can be used (interpreted as locations within [0, 1] viewport)
verticalBrush <- list(x=c(-.001, -.001, .001, .001),
                      y=c(-1, 1, 1, -1))

circleBrush <- function(n=50) {
    t <- seq(0, 2*pi, length.out=n)[-n]
    list(x=cos(t), y=sin(t))
}

squareBrush <- list(x=c(-1, -1, 1, 1), y=c(-1, 1, 1, -1))

