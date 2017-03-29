
## Draw a line with variable width by generating line segments with
## specified width and then adding corner joins and line ends
grid.vwline <- function(...) {
    grid.draw(vwlineGrob(...))
}

## IF open=FALSE, endShape and endWidth are IGNORED
vwlineGrob <- function(x, y, w, default.units="npc", open=TRUE, angle="perp",
                       linejoin="round", lineend="butt", mitrelimit=4,
                       stepWidth=FALSE, render=vwPolygon,
                       gp=gpar(fill="black"), name=NULL, debug=FALSE) {
    if (!is.unit(x)) {
        x <- unit(x, default.units)
    }
    if (!is.unit(y)) {
        y <- unit(y, default.units)
    }
    if (!inherits(w, "widthSpec")) {
        w <- widthSpec(w, default.units)
    }
    checkvwline(x, y, w)
    gTree(x=x, y=y, w=w, open=open, render=render, angle=angle,
          linejoin=linejoin, lineend=lineend, mitrelimit=mitrelimit,
          stepWidth=stepWidth,
          gp=gp, name=name, cl="vwlineGrob",
          debug=debug)
}

checkvwline <- function(x, y, w) {
    if (max(length(x), length(y), length(w)) < 2)
        stop("A vwline must have at least two points")
    nx <- length(x)
    ny <- length(y)
    nw <- length(w)
    if (nx != ny || nx != nw) {
        stop("x, y, and w must all have same length")
    }
}

buildEdge <- function(perpStart, perpEnd, inside, mitrelen, mitrelimit,
                      intpt1, intpt2, arc, join, leftedge) {
    N <- length(perpStart)
    x <- vector("list", N+1)
    x[[1]] <- perpStart[1]
    if (N > 1) {
        for (i in 1:(N-1)) {
            if (inside[i]) {
                x[[i+1]] <- c(intpt1[i], intpt2[i])
            } else {
                switch(join,
                       round=
                           {
                               if (leftedge) {
                                   x[[i+1]] <- c(perpEnd[i], arc[[i]],
                                                 perpStart[i+1])
                               } else {
                                   x[[i+1]] <- c(perpEnd[i], rev(arc[[i]]),
                                                 perpStart[i+1])
                               }
                           },
                       mitre=
                           {
                               if (mitrelen < mitrelimit) {
                                   x[[i+1]] <- c(intpt1[i], intpt2[i])
                               } else {
                                   ## Fall back to bevel
                                   x[[i+1]] <- c(perpEnd[i], perpStart[i+1])
                               }
                           },
                       bevel=
                           {
                               x[[i+1]] <- c(perpEnd[i], perpStart[i+1])
                           },
                       stop("Invalid linejoin value")
                       )
            }
        }
    }
    x[[N+1]] <- perpEnd[N]
    unlist(x)
}

## Calculate points to the left and to the right
vwlinePoints <- function(grob) {
    x <- convertX(grob$x, "in", valueOnly=TRUE)
    y <- convertY(grob$y, "in", valueOnly=TRUE)
    w <- grob$w
    w$left <- pmin(convertWidth(w$left, "in", valueOnly=TRUE),
                   convertHeight(w$left, "in", valueOnly=TRUE))
    w$right <- pmin(convertWidth(w$right, "in", valueOnly=TRUE),
                    convertHeight(w$right, "in", valueOnly=TRUE))
    sinfo <- segInfo(x, y, w, grob$stepWidth, grob$debug)
    cinfo <- cornerInfo(x, y, sinfo, grob$stepWidth, grob$debug)
    carcinfo <- cornerArcInfo(sinfo, cinfo, grob$debug)
    leftx <- buildEdge(sinfo$perpStartLeftX,
                       sinfo$perpEndLeftX,
                       cinfo$leftInside,
                       cinfo$leftMitreLength, grob$mitrelimit,
                       cinfo$leftIntx1,
                       cinfo$leftIntx2,
                       carcinfo$leftarcx,
                       grob$linejoin, TRUE)
    lefty <- buildEdge(sinfo$perpStartLeftY,
                       sinfo$perpEndLeftY,
                       cinfo$leftInside,
                       cinfo$leftMitreLength, grob$mitrelimit,
                       cinfo$leftInty1,
                       cinfo$leftInty2,
                       carcinfo$leftarcy,
                       grob$linejoin, TRUE)
    rightx <- buildEdge(rev(sinfo$perpEndRightX),
                        rev(sinfo$perpStartRightX),
                        rev(cinfo$rightInside),
                        rev(cinfo$rightMitreLength), grob$mitrelimit,
                        rev(cinfo$rightIntx2),
                        rev(cinfo$rightIntx1),
                        rev(carcinfo$rightarcx),
                        grob$linejoin, FALSE)
    righty <- buildEdge(rev(sinfo$perpEndRightY),
                        rev(sinfo$perpStartRightY),
                        rev(cinfo$rightInside),
                        rev(cinfo$rightMitreLength), grob$mitrelimit,
                        rev(cinfo$rightInty2),
                        rev(cinfo$rightInty1),
                        rev(carcinfo$rightarcy),
                        grob$linejoin, FALSE)
    list(left=list(x=leftx, y=lefty),
         right=list(x=rightx, y=righty),
         sinfo=sinfo)
}

buildEnds <- function(w, einfo, earcinfo, stepWidth,
                      linejoin, lineend, mitrelimit) {
    switch(lineend,
           butt=
               {
                   startx <- starty <- endx <- endy <- numeric()
               },
           round=
               {
                   startx <- earcinfo[[1]]$arcx[[1]]
                   starty <- earcinfo[[1]]$arcy[[1]]
                   endx <- earcinfo[[2]]$arcx[[1]]
                   endy <- earcinfo[[2]]$arcy[[1]]
               },
           mitre=
               {
                   width <- w$left[1] + w$right[1]
                   if (width > 0 &&
                       einfo$startmitrelength/width <= mitrelimit &&
                       ## Not diverging at end
                       abs(earcinfo[[1]]$cornerangle) < pi) {
                       startx <- einfo$startmitre$x
                       starty <- einfo$startmitre$y
                   } else {
                       ## Fall back to square
                       startx <- c(einfo$startcorner2$x, einfo$startcorner1$x)
                       starty <- c(einfo$startcorner2$y, einfo$startcorner1$y)
                   }
                   N <- length(w$left)
                   if (stepWidth) {
                       width <- w$left[N-1] + w$right[N-1]
                   } else {
                       width <- w$left[N] + w$right[N]
                   }
                   if (width > 0 &&
                       einfo$endmitrelength/width <= mitrelimit &&
                       ## Not diverging at end
                       abs(earcinfo[[2]]$cornerangle) < pi) {
                       endx <- einfo$endmitre$x
                       endy <- einfo$endmitre$y
                   } else {
                       endx <- c(einfo$endcorner1$x, einfo$endcorner2$x)
                       endy <- c(einfo$endcorner1$y, einfo$endcorner2$y)
                   }
               },
           square=
               {
                   startx <- c(einfo$startcorner2$x, einfo$startcorner1$x)
                   starty <- c(einfo$startcorner2$y, einfo$startcorner1$y)
                   endx <- c(einfo$endcorner1$x, einfo$endcorner2$x)
                   endy <- c(einfo$endcorner1$y, einfo$endcorner2$y)
               },
           stop("Invalid lineend value")
           )
    list(startx=startx, starty=starty, endx=endx, endy=endy)
}

vwlineOutline <- function(grob) {
    x <- convertX(grob$x, "in", valueOnly=TRUE)
    y <- convertY(grob$y, "in", valueOnly=TRUE)
    w <- grob$w
    w$left <- pmin(convertWidth(w$left, "in", valueOnly=TRUE),
                   convertHeight(w$left, "in", valueOnly=TRUE))
    w$right <- pmin(convertWidth(w$right, "in", valueOnly=TRUE),
                    convertHeight(w$right, "in", valueOnly=TRUE))
    pts <- vwlinePoints(grob)
    sinfo <- segInfo(x, y, w, grob$stepWidth, grob$debug)
    einfo <- endInfo(x, y, w, sinfo, grob$stepWidth, grob$debug)
    earcinfo <- endArcInfo(sinfo, einfo, grob$debug)
    ends <- buildEnds(w, einfo, earcinfo, grob$stepWidth,
                      grob$linejoin, grob$lineend, grob$mitrelimit)
    list(x=c(ends$startx, pts$left$x, ends$endx, pts$right$x),
         y=c(ends$starty, pts$left$y, ends$endy, pts$right$y))
}

makeContent.vwlineGrob <- function(x, ...) {
    outline <- vwlineOutline(x)
    addGrob(x,
            x$render(outline$x, outline$y, length(outline$x), x$gp, "outline"))
}

edgePoints.vwlineGrob <- function(x, d,
                                  which=c("left", "right"),
                                  direction="forward",
                                  debug=FALSE,
                                  ...) {
    pts <- vwlinePoints(x)
    result <- list(left=NULL, right=NULL)
    if ("left" %in% which) {
        result$left=vwEdgePoints(pts$left, d, direction == "forward",
                                 x$open, debug)
    }
    if ("right" %in% which) {
        result$right=vwEdgePoints(pts$right, d, direction == "forward",
                                  x$open, debug)
    }
    result
}

