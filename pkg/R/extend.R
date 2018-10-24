
## Code for calculating ends and joins in style "extend"
## for offsetBezierGrob with BezierWidth width

## Function to calculate extension amount in terms of 't'
## to extend by an amount in inches 'd'
## (based on the assumption that steps along extension, per unit 't',
##  will always be longer than steps within curve, 0 < t < 1,
##  so determining distance per unit 't' within curve will be lower
##  bound on distance per unit 't' along extension)
calcExtensionT <- function(d, length) {
    d/length
}

extendJoins <- function(pts1, pts2, w1, w2, opts1, lengths1, lengths2,
                        i1, i2, grob, widths) {
    x <- convertX(grob$x, "in", valueOnly=TRUE)
    y <- convertY(grob$y, "in", valueOnly=TRUE)
    bez1 <- BezierGrob(x[i1], y[i1], default.units="in", stepFn=grob$stepFn)
    bez2 <- BezierGrob(x[i2], y[i2], default.units="in", stepFn=grob$stepFn)
    ## Calculate corner info
    n1 <- length(pts1$x)
    sinfo <- segInfo(c(pts1$x[n1-1], pts2$x[1:2]),
                     c(pts1$y[n1-1], pts2$y[1:2]),
                     list(left=c(w1[n1-1], w2[1:2]),
                          right=c(w1[n1-1], w2[1:2])),
                     TRUE, FALSE, grob$debug)
    cinfo <- cornerInfo(sinfo, TRUE, FALSE, grob$debug)
    ## How far to extend ?
    joinExtend <- calcExtensionT(grob$mitrelimit*w2[1]*2,
                                 c(sum(lengths1), sum(lengths2)))
    ## Extend curve 1
    extpts1 <- BezierPoints(bez1, c(1, 1 + joinExtend[1]))
    extopts1 <- BezierNormal(bez1, c(1, 1 + joinExtend[1]))
    extlengths1 <- c(0, sqrt(diff(extpts1$x)^2 + diff(extpts1$y)^2))
    extcumLength1 <- cumsum(extlengths1)
    ## lengths1 + cumLength because lengths are AFTER curve1
    extww1 <- approx(widths$x, widths$y, lengths1 + extcumLength1, rule=2)$y/2
    ## Extend curve 2
    extpts2 <- BezierPoints(bez2, c(0, -joinExtend[2]))
    extopts2 <- BezierNormal(bez2, c(0, -joinExtend[2]))
    extlengths2 <- c(0, sqrt(diff(extpts2$x)^2 + diff(extpts2$y)^2))
    extcumLength2 <- cumsum(extlengths2)
    ## lengths1 - cumLength because lengths are BEFORE curve 2
    extww2 <- approx(widths$x, widths$y, lengths1 - extcumLength2, rule=2)$y/2
    ## Do the "outside" edges intersect ?
    if (cinfo$leftInside) {
        edge1x <- extpts1$x + extww1*extopts1$x
        edge1y <- extpts1$y + extww1*extopts1$y
        edge2x <- extpts2$x + extww2*extopts2$x
        edge2y <- extpts2$y + extww2*extopts2$y
        n1 <- length(edge1x)
        n2 <- length(edge2x)
        intersections <- mapply(
            function(sx1, sy1, ex1, ey1, sx2, sy2, ex2, ey2) {
                intpts <- intersection(sx1, sy1, ex1, ey1, sx2, sy2, ex2, ey2)
                onSegment(sx1, sy1, intpts$x, intpts$y, ex1, ey1) &
                    onSegment(sx2, sy2, intpts$x, intpts$y, ex2, ey2)
            },
            edge1x[-n1], edge1y[-n1],
            edge1x[-1], edge1y[-1],
            MoreArgs=list(edge2x[-n2], edge2y[-n2],
                          edge2x[-1], edge2y[-1]))
        if (!any(intersections)) {
            ## Fall back to "bevel"
            corner <- list(extend=FALSE)
        } else {
            intseg <- which(intersections, arr.ind=TRUE)[1,]
            keep1 <- 1:(intseg[2])
            keep2 <- 1:(intseg[1])
            int <- intersection(edge1x[intseg[2]], edge1y[intseg[2]],
                                edge1x[intseg[2] + 1], edge1y[intseg[2] + 1],
                                edge2x[intseg[1]], edge2y[intseg[1]],
                                edge2x[intseg[1] + 1], edge2y[intseg[1] + 1])
            corner <- list(extend=TRUE,
                           leftInside=TRUE,
                           x=c(edge1x[keep1], int$x, edge2x[keep2]),
                           y=c(edge1y[keep1], int$y, edge2y[keep2]))
        }
    } else {
        edge1x <- extpts1$x - extww1*extopts1$x
        edge1y <- extpts1$y - extww1*extopts1$y
        edge2x <- extpts2$x - extww2*extopts2$x
        edge2y <- extpts2$y - extww2*extopts2$y
        n1 <- length(edge1x)
        n2 <- length(edge2x)
        intersections <- mapply(
            function(sx1, sy1, ex1, ey1, sx2, sy2, ex2, ey2) {
                intpts <- intersection(sx1, sy1, ex1, ey1, sx2, sy2, ex2, ey2)
                onSegment(sx1, sy1, intpts$x, intpts$y, ex1, ey1) &
                    onSegment(sx2, sy2, intpts$x, intpts$y, ex2, ey2)
            },
            edge1x[-n1], edge1y[-n1],
            edge1x[-1], edge1y[-1],
            MoreArgs=list(edge2x[-n2], edge2y[-n2],
                          edge2x[-1], edge2y[-1]))
        if (!any(intersections)) {
            ## Fall back to "bevel"
            corner <- list(extend=FALSE)
        } else {
            intseg <- which(intersections, arr.ind=TRUE)
            keep1 <- 1:(intseg[2])
            keep2 <- 1:(intseg[1])
            int <- intersection(edge1x[intseg[2]], edge1y[intseg[2]],
                                edge1x[intseg[2] + 1], edge1y[intseg[2] + 1],
                                edge2x[intseg[1]], edge2y[intseg[1]],
                                edge2x[intseg[1] + 1], edge2y[intseg[1] + 1])
            corner <- list(extend=TRUE,
                           leftInside=TRUE,
                           x=c(edge1x[keep1], int$x, edge2x[keep2]),
                           y=c(edge1y[keep1], int$y, edge2y[keep2]))
        }
    }
    if (grob$debug) {
        grid.points(extpts1$x, extpts1$y, default.units="in", pch=".",
                    gp=gpar(col="black"))
        grid.lines(extpts1$x - extww1*extopts1$x, extpts1$y - extww1*extopts1$y,
                   default.units="in", gp=gpar(col="blue"))
        grid.lines(extpts1$x + extww1*extopts1$x, extpts1$y + extww1*extopts1$y,
                   default.units="in", gp=gpar(col="red"))
        grid.points(extpts2$x, extpts2$y, default.units="in", pch=".",
                    gp=gpar(col="black"))
        grid.lines(extpts2$x - extww2*extopts2$x, extpts2$y - extww2*extopts2$y,
                   default.units="in", gp=gpar(col="blue"))
        grid.lines(extpts2$x + extww2*extopts2$x, extpts2$y + extww2*extopts2$y,
                   default.units="in", gp=gpar(col="red"))
    }
    corner
}

extendEnds <- function(grob, pts) {
    x <- convertX(grob$x, "in", valueOnly=TRUE)
    y <- convertY(grob$y, "in", valueOnly=TRUE)
    ncurves <- (length(x) - 1) %/% 3    
    ## First Bezier curve
    bezFirst <- BezierGrob(x[1:4], y[1:4],
                           default.units="in",
                           stepFn=grob$stepFn)
    firstPts <- BezierPoints(bezFirst)
    firstLengths <- c(0, sqrt(diff(firstPts$x)^2 + diff(firstPts$y)^2))
    cumFirstLength <- cumsum(firstLengths)
    totalFirstLength <- sum(firstLengths)
    ## How far to extend end ?
    ## (if end diverging, we extend by half width at end)
    ## (otherwise, extend at most by mitrelength)
    startWidth <- pts$info$ww[1]*2
    startMitreLimit <- startWidth*grob$mitrelimit
    startExtend <- calcExtensionT(c(startMitreLimit, startWidth/2),
                                  totalFirstLength)
    ## Extend end
    startpts <- BezierPoints(bezFirst, c(0, -startExtend[1]))
    startopts <- BezierNormal(bezFirst, c(0, -startExtend[1]))
    ## Calculate widths based on main curve length 
    startWidthExtend <- calcExtensionT(c(startMitreLimit, startWidth/2),
                                       pts$info$totalLength)
    startWidths <- resolveWidth(grob$w, pts$info$totalLength,
                                c(0, -startWidthExtend[1]), fill=FALSE)
    ## If the width is not negative by this point, fall back to "square" end
    if (startWidths$y[length(startWidths$y)] > 0) {
        startpts <- BezierPoints(bezFirst, c(0, -startExtend[2]))
        startopts <- BezierNormal(bezFirst, c(0, -startExtend[2]))
        ## Calculate widths based on main curve length 
        startWidths <- resolveWidth(grob$w, pts$info$totalLength,
                                    c(0, -startWidthExtend[2]), fill=FALSE)
        startLengths <- c(0, sqrt(diff(startpts$x)^2 + diff(startpts$y)^2))
        cumStartLength <- cumsum(startLengths)
        startw <- approx(startWidths$x, startWidths$y,
                         -cumStartLength, rule=2)$y/2
        keep <- cumStartLength <= startWidth/2
        startpts$x <- startpts$x[keep]
        startpts$y <- startpts$y[keep]
        startopts$x <- startopts$x[keep]
        startopts$y <- startopts$y[keep]
        startw <- startw[keep]
    } else {
        ## Interpolate width at each vertex
        ## (divide by 2 because width is added to both left and right)
        ## This interpolation must occur on extended end
        startLengths <- c(0, sqrt(diff(startpts$x)^2 + diff(startpts$y)^2))
        cumStartLength <- cumsum(startLengths)
        ## -cumLength because lengths are BEFORE curve start
        startw <- approx(startWidths$x, startWidths$y,
                         -cumStartLength, rule=2)$y/2
        keep <- startw >= 0
        startpts$x <- startpts$x[keep]
        startpts$y <- startpts$y[keep]
        startopts$x <- startopts$x[keep]
        startopts$y <- startopts$y[keep]
        startw <- startw[keep]
    }

    ## Last Bezier curve
    n <- length(x)
    bezLast <- BezierGrob(x[(n-3):n], y[(n-3):n], 
                          default.units="in",
                          stepFn=grob$stepFn)
    lastPts <- BezierPoints(bezLast)
    lastLengths <- c(0, sqrt(diff(lastPts$x)^2 + diff(lastPts$y)^2))
    cumLastLength <- cumsum(lastLengths)
    totalLastLength <- sum(lastLengths)
    ## How far to extend end ?
    ## (if end diverging, we extend by half width at end)
    ## (otherwise, extend at most by mitrelength)
    endWidth <- pts$info$ww[length(pts$info$ww)]*2
    endMitreLimit <- endWidth*grob$mitrelimit
    endExtend <- calcExtensionT(c(endMitreLimit, endWidth/2),
                                totalLastLength)
    ## Extend end
    endpts <- BezierPoints(bezLast, c(1, 1 + endExtend[1]))
    endopts <- BezierNormal(bezLast, c(1, 1 + endExtend[1]))
    ## Calculate widths based on main curve length 
    endWidthExtend <- calcExtensionT(c(endMitreLimit, endWidth/2),
                                     pts$info$totalLength)
    endWidths <- resolveWidth(grob$w, pts$info$totalLength,
                              nWidthCurves(grob$w) + c(0, endWidthExtend[1]),
                              fill=FALSE)
    ## If the width is not negative by this point, fall back to "square" end
    if (endWidths$y[length(endWidths$y)] > 0) {
        endpts <- BezierPoints(bezLast, c(1, 1 + endExtend[2]))
        endopts <- BezierNormal(bezLast, c(1, 1 + endExtend[2]))
        ## Calculate widths based on main curve length 
        endWidths <- resolveWidth(grob$w, pts$info$totalLength,
                                  nWidthCurves(grob$w) +
                                  c(0, endWidthExtend[2]),
                                  fill=FALSE)
        endLengths <- c(0, sqrt(diff(endpts$x)^2 + diff(endpts$y)^2))
        cumEndLength <- cumsum(endLengths)
        endw <- approx(endWidths$x, endWidths$y,
                       pts$info$totalLength + cumEndLength, rule=2)$y/2
        keep <- cumEndLength <= endWidth/2
        endpts$x <- endpts$x[keep]
        endpts$y <- endpts$y[keep]
        endopts$x <- endopts$x[keep]
        endopts$y <- endopts$y[keep]
        endw <- endw[keep]
    } else {
        ## Interpolate width at each vertex
        ## (divide by 2 because width is added to both left and right)
        ## This interpolation must occur on extended end
        endLengths <- c(0, sqrt(diff(endpts$x)^2 + diff(endpts$y)^2))
        cumEndLength <- cumsum(endLengths)
        ## cumLength + totalLength because lengths are AFTER curve end
        endw <- approx(endWidths$x, endWidths$y,
                       pts$info$totalLength + cumEndLength, rule=2)$y/2
        keep <- endw >= 0
        endpts$x <- endpts$x[keep]
        endpts$y <- endpts$y[keep]
        endopts$x <- endopts$x[keep]
        endopts$y <- endopts$y[keep]
        endw <- endw[keep]
    }
    if (grob$debug) {
        grid.points(startpts$x, startpts$y, default.units="in", pch=".",
                    gp=gpar(col="black"))
        grid.lines(startpts$x - startw*startopts$x,
                   startpts$y - startw*startopts$y,
                   default.units="in", gp=gpar(col="blue"))
        grid.lines(startpts$x + startw*startopts$x,
                   startpts$y + startw*startopts$y,
                   default.units="in", gp=gpar(col="red"))
        grid.points(endpts$x, endpts$y, default.units="in", pch=".",
                    gp=gpar(col="black"))
        grid.lines(endpts$x - endw*endopts$x, endpts$y - endw*endopts$y,
                   default.units="in", gp=gpar(col="blue"))
        grid.lines(endpts$x + endw*endopts$x, endpts$y + endw*endopts$y,
                   default.units="in", gp=gpar(col="red"))
    }
    list(startx=c(startpts$x + startw*startopts$x,
                  rev(startpts$x - startw*startopts$x)),
         starty=c(startpts$y + startw*startopts$y,
                  rev(startpts$y - startw*startopts$y)),
         endx=c(endpts$x - endw*endopts$x,
                rev(endpts$x + endw*endopts$x)),
         endy=c(endpts$y - endw*endopts$y,
                rev(endpts$y + endw*endopts$y)))
}
