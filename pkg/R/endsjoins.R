## Generate a bunch of useful information about the line
## (NOTE that width is allowed to be different to the left and to the right
##  of the main line)
## 'x' and 'y' must have same length
segInfo <- function(x, y, w, stepWidth=FALSE, debug=FALSE) {
    N <- length(x)
    ## All of these are per *segment* (N - 1)
    dx <- diff(x)
    dy <- diff(y)
    lengths <- dist(dx^2, dy^2)
    angle <- atan2(dy, dx)
    perpangle <- angleInRange(angle + pi/2)
    cosperp <- cos(perpangle)
    sinperp <- sin(perpangle)

    if (stepWidth) {
        perpStartLeftX <- x[-N] + w$left[-N]*cosperp
        perpStartLeftY <- y[-N] + w$left[-N]*sinperp
        perpStartRightX <- x[-N] - w$right[-N]*cosperp
        perpStartRightY <- y[-N] - w$right[-N]*sinperp
        perpEndLeftX <- x[-1] + w$left[-N]*cosperp
        perpEndLeftY <- y[-1] + w$left[-N]*sinperp
        perpEndRightX <- x[-1] - w$right[-N]*cosperp
        perpEndRightY <- y[-1] - w$right[-N]*sinperp        
    } else {
        perpStartLeftX <- x[-N] + w$left[-N]*cosperp
        perpStartLeftY <- y[-N] + w$left[-N]*sinperp
        perpStartRightX <- x[-N] - w$right[-N]*cosperp
        perpStartRightY <- y[-N] - w$right[-N]*sinperp
        perpEndLeftX <- x[-1] + w$left[-1]*cosperp
        perpEndLeftY <- y[-1] + w$left[-1]*sinperp
        perpEndRightX <- x[-1] - w$right[-1]*cosperp
        perpEndRightY <- y[-1] - w$right[-1]*sinperp
    }
    
    if (debug) {
        pts(x, y)
        lines(x, y)
        polyg(c(perpStartLeftX, perpEndLeftX,
                perpEndRightX, perpStartRightX),
              c(perpStartLeftY, perpEndLeftY,
                perpEndRightY, perpStartRightY),
              id=rep(1:(N-1), 4), "grey")
        segs(x[-N], y[-N], perpStartLeftX, perpStartLeftY, "red")
        segs(x[-1], y[-1], perpEndLeftX, perpEndLeftY, "pink")
        segs(x[-N], y[-N], perpStartRightX, perpStartRightY, "blue")
        segs(x[-1], y[-1], perpEndRightX, perpEndRightY, "lightblue")
    }
    
    data.frame(dx, dy, lengths, angle, perpangle, cosperp, sinperp,
               perpStartLeftX, perpStartLeftY, perpStartRightX, perpStartRightY,
               perpEndLeftX, perpEndLeftY, perpEndRightX, perpEndRightY)
}

cornerInfo <- function(x, y, sinfo, stepWidth=FALSE, debug=FALSE) {
    N <- nrow(sinfo)
    if (N < 2) return(data.frame())
    ## All of these are per *corner* (N - 1)
    with(sinfo,
         {
             leftAngle <- angleDiff(angle[-N], angle[-1], clockwise=FALSE)
             rightAngle <- angleDiff(angle[-N], angle[-1], clockwise=TRUE)
             leftInside <- leftAngle < pi
             ## Following PDF definition
             leftMitreLength <-
                 ifelse(leftInside,
                        0, 
                        1/sin(angleDiff(angleInRange(angle[-N] + pi),
                                        angle[-1], clockwise=FALSE)/2))
             rightMitreLength <-
                 ifelse(leftInside, 
                        1/sin(angleDiff(angleInRange(angle[-N] + pi),
                                        angle[-1], clockwise=TRUE)/2),
                        0)
             
             ## Intersection left edge segments with each other
             leftIntEdge <- intersection(perpStartLeftX[-N], perpStartLeftY[-N],
                                         perpEndLeftX[-N], perpEndLeftY[-N],
                                         perpStartLeftX[-1], perpStartLeftY[-1],
                                         perpEndLeftX[-1], perpEndLeftY[-1])
             ## Intersection left edge segments with next segment end
             leftInt1 <- intersection(perpStartLeftX[-N], perpStartLeftY[-N],
                                      perpEndLeftX[-N], perpEndLeftY[-N],
                                      perpStartLeftX[-1], perpStartLeftY[-1],
                                      perpStartRightX[-1], perpStartRightY[-1])
             ## Intersection left edge segments with prev segment end
             leftInt2 <- intersection(perpStartLeftX[-1], perpStartLeftY[-1],
                                      perpEndLeftX[-1], perpEndLeftY[-1],
                                      perpEndLeftX[-N], perpEndLeftY[-N],
                                      perpEndRightX[-N], perpEndRightY[-N])
             ## Edge segments intersect on edge segment ?
             edgeInt1 <- onSegment(perpStartLeftX[-N], perpStartLeftY[-N],
                                   leftIntEdge$x, leftIntEdge$y,
                                   perpEndLeftX[-N], perpEndLeftY[-N])
             edgeInt2 <- onSegment(perpStartLeftX[-1], perpStartLeftY[-1],
                                   leftIntEdge$x, leftIntEdge$y,
                                   perpEndLeftX[-1], perpEndLeftY[-1])
             ## Edge segments intersect on at least one edge segment
             bothEdgesInt <- edgeInt1 & edgeInt2
             edgesInt <- edgeInt1 | edgeInt2
             ## Edge segment intersects with next segment end on segment end
             edgeIntNext <- onSegment(perpStartLeftX[-1], perpStartLeftY[-1],
                                      leftInt1$x, leftInt1$y,
                                      perpStartRightX[-1], perpStartRightY[-1])
             edgeIntPrev <- onSegment(perpStartLeftX[-N], perpStartLeftY[-N],
                                      leftInt2$x, leftInt2$y,
                                      perpStartRightX[-N], perpStartRightY[-N])
             ## Is the intersection with next segment end between edge end
             ## and edge intersection ?
             endIntBetween <- onSegment(perpEndLeftX[-N], perpEndLeftY[-N],
                                        leftInt1$x, leftInt1$y,
                                        leftIntEdge$x, leftIntEdge$y) |
                              onSegment(perpStartLeftX[-1], perpStartLeftY[-1],
                                        leftInt2$x, leftInt2$y,
                                        leftIntEdge$x, leftIntEdge$y)
             useEdgeInt <- (leftInside & bothEdgesInt) |
                 (leftInside & edgesInt & !stepWidth) |
                 (!leftInside & !endIntBetween)
                        
             leftIntx1 <-
                 ifelse(useEdgeInt,
                        leftIntEdge$x,
                        ifelse(edgeIntNext, leftInt1$x, perpEndLeftX[-N]))
             leftIntx2 <-
                 ifelse(useEdgeInt,
                        leftIntEdge$x,
                        ifelse(edgeIntNext, perpStartLeftX[-1], leftInt2$x))
             leftInty1 <-
                 ifelse(useEdgeInt,
                        leftIntEdge$y,
                        ifelse(edgeIntNext, leftInt1$y, perpEndLeftY[-N]))
             leftInty2 <-
                 ifelse(useEdgeInt,
                        leftIntEdge$y,
                        ifelse(edgeIntNext, perpStartLeftY[-1], leftInt2$y))
             
             rightInside <- !leftInside
             rightIntEdge <- intersection(perpStartRightX[-N],
                                        perpStartRightY[-N],
                                        perpEndRightX[-N], perpEndRightY[-N],
                                        perpStartRightX[-1],
                                        perpStartRightY[-1],
                                        perpEndRightX[-1], perpEndRightY[-1])
             rightInt1 <- intersection(perpStartRightX[-N], perpStartRightY[-N],
                                      perpEndRightX[-N], perpEndRightY[-N],
                                      perpStartLeftX[-1], perpStartLeftY[-1],
                                      perpStartRightX[-1], perpStartRightY[-1])
             rightInt2 <- intersection(perpStartRightX[-1], perpStartRightY[-1],
                                      perpEndRightX[-1], perpEndRightY[-1],
                                      perpEndLeftX[-N], perpEndLeftY[-N],
                                      perpEndRightX[-N], perpEndRightY[-N])
             edgeInt1 <- onSegment(perpStartRightX[-N], perpStartRightY[-N],
                                   rightIntEdge$x, rightIntEdge$y,
                                   perpEndRightX[-N], perpEndRightY[-N]) 
             edgeInt2 <- onSegment(perpStartRightX[-1], perpStartRightY[-1],
                                   rightIntEdge$x, rightIntEdge$y,
                                   perpEndRightX[-1], perpEndRightY[-1])
             bothEdgesInt <- edgeInt1 & edgeInt2
             edgesInt <- edgeInt1 | edgeInt2
             edgeIntNext <- onSegment(perpStartLeftX[-1], perpStartLeftY[-1],
                                      rightInt1$x, rightInt1$y,
                                      perpStartRightX[-1], perpStartRightY[-1])
             edgeIntPrev <- onSegment(perpStartLeftX[-N], perpStartLeftY[-N],
                                      rightInt2$x, rightInt2$y,
                                      perpStartRightX[-N], perpStartRightY[-N])
             endIntBetween <- onSegment(perpEndRightX[-N], perpEndRightY[-N],
                                        leftInt1$x, rightInt1$y,
                                        rightIntEdge$x, rightIntEdge$y) |
                              onSegment(perpStartRightX[-1], perpStartRightY[-1],
                                        rightInt2$x, rightInt2$y,
                                        rightIntEdge$x, rightIntEdge$y)
             useEdgeInt <- (rightInside & bothEdgesInt) |
                 (rightInside & edgesInt & !stepWidth) |
                 (!rightInside & !endIntBetween)
             rightIntx1 <-
                 ifelse(useEdgeInt,
                        rightIntEdge$x,
                        ifelse(edgeIntNext, rightInt1$x, perpEndRightX[-N]))
             rightIntx2 <-
                 ifelse(useEdgeInt,
                        rightIntEdge$x,
                        ifelse(edgeIntNext, perpStartRightX[-1], rightInt2$x))
             rightInty1 <-
                 ifelse(useEdgeInt,
                        rightIntEdge$y,
                        ifelse(edgeIntNext, rightInt1$y, perpEndRightY[-N]))
             rightInty2 <-
                 ifelse(useEdgeInt,
                        rightIntEdge$y,
                        ifelse(edgeIntNext, perpStartRightY[-1], rightInt2$y))
             
             if (debug) {
                 pts(leftIntx1[leftInside], leftInty1[leftInside], "orange")
                 pts(leftIntx2[leftInside], leftInty2[leftInside], "orange")
                 pts(rightIntx1[rightInside], rightInty1[rightInside],
                     "orange")
                 pts(rightIntx2[rightInside], rightInty2[rightInside],
                     "orange")
                 pts(leftIntx1[!leftInside], leftInty1[!leftInside],
                     "orange")
                 pts(rightIntx1[!rightInside], rightInty1[!rightInside],
                     "orange")
                 pts(leftIntx2[!leftInside], leftInty2[!leftInside],
                     "orange")
                 pts(rightIntx2[!rightInside], rightInty2[!rightInside],
                     "orange")
                 polyl(c(perpEndLeftX[-N][!leftInside],
                         leftIntx1[!leftInside], leftIntx2[!leftInside],
                         perpStartLeftX[-1][!leftInside]),
                       c(perpEndLeftY[-N][!leftInside],
                         leftInty1[!leftInside], leftInty2[!leftInside],
                         perpStartLeftY[-1][!leftInside]),
                       id=rep((1:(N-1))[!leftInside], 4), "orange")
                 polyl(c(perpEndRightX[-N][!rightInside],
                         rightIntx1[!rightInside], rightIntx2[!rightInside],
                         perpStartRightX[-1][!rightInside]),
                       c(perpEndRightY[-N][!rightInside],
                         rightInty1[!rightInside], rightInty2[!rightInside],
                         perpStartRightY[-1][!rightInside]),
                       id=rep((1:(N-1))[!rightInside], 4), "orange")
             }

             data.frame(leftInside, rightInside,
                        leftMitreLength, rightMitreLength,
                        leftIntx1, leftIntx2, leftInty1, leftInty2,
                        rightIntx1, rightIntx2, rightInty1, rightInty2)
         })
}

bezierArcInfo <- function(startx, starty, endx, endy, inside, leftedge,
                          N, angle, centre, cornerangle, nsteps,
                          isjoin, debug) {
    ## If we are on a join, must ensure that angles are clockwise
    ## around left corners and anticlockwise around right corners
    ## (especially when the join becomes inverted)
    ## Ends are not the same, just keep going around a wide end 
    if (isjoin) {
        cornerangle <- angleInRange(cornerangle)
    }
    onedegree <- 1/180*pi
    k <- ifelse(abs(cornerangle - pi) < onedegree |
                abs(cornerangle - -pi) < onedegree,
                2/3, abs(4/(3*(1/cos(cornerangle/2) + 1))))
    int <- intersection(startx[-N], starty[-N],
                        endx[-N], endy[-N],
                        startx[-1], starty[-1],
                        endx[-1], endy[-1])
    ## Special case angle pi (where 'k' calculation breaks down [div by zero])
    ## Also special case where corner has point of inflexion
    ## (so incident edges do not intersect between edge ends;
    ##  just use half dist between edge ends)
    len <- ifelse(abs(cornerangle - pi) < onedegree |
                  abs(cornerangle - -pi) < onedegree,
                  dist(endx[-N] - startx[-1], endy[-N] - starty[-1]),
                  ifelse(onSegment(startx[-N], starty[-N], int$x, int$y,
                                   endx[-N], endy[-N]) |
                         onSegment(startx[-1], starty[-1], int$x, int$y,
                                   endx[-1], endy[-1]),
                         dist(endx[-N] - startx[-1], endy[-N] - starty[-1])/2,
                         pmin(dist(int$x - endx[-N], int$y - endy[-N]),
                              dist(int$x - startx[-1], int$y - starty[-1]))))
    
    cp1 <- extend(endx[-N], endy[-N], angle[-N], k*len)
    cp2 <- extend(startx[-1], starty[-1], angle[-1], -k*len)

    arcs <- vector("list", N-1)
    subset <- !inside & nsteps > 0
    if (any(subset)) {
        for (i in (1:(N-1))[subset]) {
            bezg <- bezierGrob(c(endx[i], cp1$x[i], cp2$x[i], startx[i+1]),
                               c(endy[i], cp1$y[i], cp2$y[i], starty[i+1]),
                               default.units="in")
            arcs[[i]] <- bezierPoints(bezg)
        }
    }
    
    if (debug) {
        if (any(subset)) {
            for (i in (1:(N-1))[subset]) {
                segs(c(endx[i], startx[i+1]), c(endy[i], starty[i+1]),
                     c(cp1$x[i], cp2$x[i]), c(cp1$y[i], cp2$y[i]), "green")
                grid.bezier(c(endx[i], cp1$x[i], cp2$x[i], startx[i+1]),
                            c(endy[i], cp1$y[i], cp2$y[i], starty[i+1]),
                            default.units="in",
                            gp=gpar(col="green"))
            }
        }
    }

    arcx <- lapply(arcs, "[[", "x")
    arcy <- lapply(arcs, "[[", "y")
    data.frame(I(arcx), I(arcy), cornerangle)
}

arcInfo <- function(startx, starty, endx, endy, inside, leftedge, isjoin,
                    debug) {
    N <- length(startx)
    ## All of these are per *corner* (N - 1)
    dx <- endx - startx
    dy <- endy - starty
    angle <- atan2(dy, dx)
    if (leftedge) {
        perpangle <- angleInRange(angle + pi/2)
    } else {
        perpangle <- angleInRange(angle - pi/2)
    }        
    cosperp <- cos(perpangle)
    sinperp <- sin(perpangle)
    ## "width" of perp does not matter; just generating line segment
    wedge1PerpX <- endx[-N] + .1*cosperp[-N]
    wedge1PerpY <- endy[-N] + .1*sinperp[-N]
    wedge2PerpX <- startx[-1] + .1*cosperp[-1]
    wedge2PerpY <- starty[-1] + .1*sinperp[-1]
    centre <- intersection(endx[-N], endy[-N],
                           wedge1PerpX, wedge1PerpY,
                           startx[-1], starty[-1],
                           wedge2PerpX, wedge2PerpY)
    rad1 <- dist(endx[-N] - centre$x, endy[-N] - centre$y)
    rad2 <- dist(startx[-1] - centre$x, starty[-1] - centre$y)
    adiff <- angleDiff(perpangle[-N], perpangle[-1], leftedge)
    ## Just approximate using circle with largest radius
    arclength <- abs(pmax(rad1, rad2)*adiff)
    tol <- .01
    nsteps <- ifelse(arclength < tol, 0, arclength %/% tol)

    bezInfo <- bezierArcInfo(startx, starty, endx, endy, inside, leftedge, 
                             N, angle, centre, adiff, nsteps, isjoin,
                             debug)

    if (debug) {
        subset <- !inside
        if (any(subset)) {
            mapply(function(ax, ay, cx, cy) {
                       ## If width is zero, ax will be empty
                       if (length(ax)) {
                           polyg(c(ax, cx), c(ay, cy),
                                 col=NA, fill=rgb(0,1,0,.2))
                       }
                   },
                   bezInfo$arcx[subset], bezInfo$arcy[subset],
                   as.list(centre$x[subset]), as.list(centre$y[subset]))
        }
    }
    
    bezInfo    
}

cornerArcInfo <- function(sinfo, cinfo, debug=FALSE) {
    N <- nrow(sinfo)
    if (N < 2) return(data.frame())
    with(sinfo,
         {
             ## NOTE that this is ONLY an arc of a circle when line width
             ## is constant either side of corner OR change in line width
             ## is identical either side of corner
             leftinfo <- arcInfo(perpStartLeftX, perpStartLeftY,
                                 perpEndLeftX, perpEndLeftY,
                                 cinfo$leftInside, TRUE, TRUE, debug=debug)
             rightinfo <- arcInfo(perpStartRightX, perpStartRightY,
                                  perpEndRightX, perpEndRightY,
                                  cinfo$rightInside, FALSE, TRUE, debug=debug)
             names(leftinfo) <- paste0("left", names(leftinfo))
             names(rightinfo) <- paste0("right", names(rightinfo))
             cbind(leftinfo, rightinfo)
         })
}

## 'x' and 'y' are length 2, with *second* value the end point 
capInfo <- function(x, y, d,
                    leftperpx1, leftperpy1, leftperpx2, leftperpy2, 
                    rightperpx1, rightperpy1, rightperpx2, rightperpy2,
                    debug) {
    if (d > 0) {
        ext <- extend(x[2], y[2], angle(x, y), d)
        perpext <- perpEnd(c(x[2], ext$x),
                           c(y[2], ext$y), 1)
        corner1 <- intersection(leftperpx1, leftperpy1,
                                leftperpx2, leftperpy2,
                                perpext[1,1], perpext[1,2],
                                perpext[2,1], perpext[2,2])
        corner2 <- intersection(rightperpx1, rightperpy1,
                                rightperpx2, rightperpy2,
                                perpext[1,1], perpext[1,2],
                                perpext[2,1], perpext[2,2])
        mitre <- intersection(leftperpx1, leftperpy1,
                              leftperpx2, leftperpy2,
                              rightperpx1, rightperpy1,
                              rightperpx2, rightperpy2)
        mitrelength <- dist(mitre$x - x[2], mitre$y - y[2])
        ## Check whether mitre is pointing "backwards" 
        mitreExt <- extend(x[2], y[2], angle(x, y), 1.1*mitrelength)
        if (onSegment(x[2], y[2], mitre$x, mitre$y, mitreExt$x, mitreExt$y) &&
            is.finite(mitre$x) && is.finite(mitre$y) &&
            dist(mitre$x - x[2], mitre$y - y[2]) < d) {
            corner1 <- mitre
            corner2 <- mitre
        }
    } else {
        corner1 <- corner2 <- mitre <- list(x=x[2], y=y[2])
        mitrelength <- 0
    }

    
    if (debug) {
        pts(c(corner1$x, corner2$x),
            c(corner1$y, corner2$y), "pink")
        lines(c(leftperpx2, corner1$x, corner2$x, rightperpx2),
              c(leftperpy2, corner1$y, corner2$y, rightperpy2),
              "pink")
        pts(mitre$x, mitre$y, "orange")
        lines(c(leftperpx2, mitre$x, rightperpx2),
              c(leftperpy2, mitre$y, rightperpy2),
              "orange")
    }
             
    list(corner1, corner2, mitre, mitrelength)
}

endInfo <- function(x, y, w, sinfo, stepWidth=FALSE, debug=FALSE) {
    N <- length(x)
    with(sinfo,
         {
             d <- (w$left[1] + w$right[1])/2
             startInfo <- capInfo(x[2:1], y[2:1], d,
                                  perpEndLeftX[1], perpEndLeftY[1],
                                  perpStartLeftX[1], perpStartLeftY[1],
                                  perpEndRightX[1], perpEndRightY[1],
                                  perpStartRightX[1], perpStartRightY[1],
                                  debug)
             if (stepWidth) {
                 d <- (w$left[N-1] + w$right[N-1])/2
             } else {
                 d <- (w$left[N] + w$right[N])/2
             }
             endInfo <- capInfo(x[(N-1):N], y[(N-1):N], d,
                                perpStartLeftX[N-1], perpStartLeftY[N-1],
                                perpEndLeftX[N-1], perpEndLeftY[N-1],
                                perpStartRightX[N-1], perpStartRightY[N-1],
                                perpEndRightX[N-1], perpEndRightY[N-1],
                                debug)
             names(startInfo) <- c("startcorner1", "startcorner2",
                                   "startmitre", "startmitrelength")
             names(endInfo) <- c("endcorner1", "endcorner2",
                                 "endmitre", "endmitrelength")
             c(startInfo, endInfo)
         })
}

endArcInfo <- function(sinfo, einfo, debug=FALSE) {
    N <- nrow(sinfo)
    with(sinfo,
         {
             startInfo <- arcInfo(c(perpEndRightX[1], perpStartLeftX[1]),
                                  c(perpEndRightY[1], perpStartLeftY[1]),
                                  c(perpStartRightX[1], perpEndLeftX[1]),
                                  c(perpStartRightY[1], perpEndLeftY[1]),
                                  FALSE, TRUE, FALSE, debug)
             endInfo <- arcInfo(c(perpStartLeftX[N], perpEndRightX[N]),
                                c(perpStartLeftY[N], perpEndRightY[N]),
                                c(perpEndLeftX[N], perpStartRightX[N]),
                                c(perpEndLeftY[N], perpStartRightY[N]),
                                FALSE, TRUE, FALSE, debug)

             list(startInfo, endInfo)
         })
}

