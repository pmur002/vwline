
## Draw a line with variable width by generating line segments with
## specified width and then adding corner joins and line ends
grid.vwline <- function(...) {
    grid.draw(vwlineGrob(...))
}

## IF open=FALSE, endShape and endWidth are IGNORED
vwlineGrob <- function(x, y, w, default.units="npc", open=TRUE, 
                       linejoin="round", lineend="butt", mitrelimit=4,
                       stepWidth=FALSE,
                       render=if (open) vwPolygon else vwPath(),
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
    gTree(x=x, y=y, w=w, open=open, render=render, 
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

buildEdge <- function(join, 
                      perpStart, perpEnd, inside, mitrelen, mitrelimit,
                      intpt1, intpt2, arc, linejoin, leftedge) {
    N <- length(perpStart)
    x <- vector("list", N+1)
    x[[1]] <- perpStart[1]
    if (N > 1) {
        for (i in 1:(N-1)) {
            if (inside[i]) {
                x[[i+1]] <- c(perpEnd[i], join[i+1], perpStart[i+1])
            } else {
                switch(linejoin,
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
                               if (mitrelen[i] < mitrelimit) {
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
    sinfo <- segInfo(x, y, w, grob$open, grob$stepWidth, grob$debug)
    cinfo <- cornerInfo(sinfo, grob$open, grob$stepWidth, grob$debug)
    carcinfo <- cornerArcInfo(sinfo, cinfo, grob$open, grob$debug)
    if (!grob$open) {
        x <- c(x, x[1])
        y <- c(y, y[1])
    }
    leftx <- buildEdge(x, 
                       sinfo$perpStartLeftX,
                       sinfo$perpEndLeftX,
                       cinfo$leftInside,
                       cinfo$leftMitreLength, grob$mitrelimit,
                       cinfo$leftIntx1,
                       cinfo$leftIntx2,
                       carcinfo$leftarcx,
                       grob$linejoin, TRUE)
    lefty <- buildEdge(y,
                       sinfo$perpStartLeftY,
                       sinfo$perpEndLeftY,
                       cinfo$leftInside,
                       cinfo$leftMitreLength, grob$mitrelimit,
                       cinfo$leftInty1,
                       cinfo$leftInty2,
                       carcinfo$leftarcy,
                       grob$linejoin, TRUE)
    rightx <- buildEdge(rev(x),
                        rev(sinfo$perpEndRightX),
                        rev(sinfo$perpStartRightX),
                        rev(cinfo$rightInside),
                        rev(cinfo$rightMitreLength), grob$mitrelimit,
                        rev(cinfo$rightIntx2),
                        rev(cinfo$rightIntx1),
                        rev(carcinfo$rightarcx),
                        grob$linejoin, FALSE)
    righty <- buildEdge(rev(y),
                        rev(sinfo$perpEndRightY),
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

buildEnds <- function(w, einfo, earcinfo, stepWidth, lineend, mitrelimit) {
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
                       is.finite(einfo$startmitrelength) &&
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
                       is.finite(einfo$endmitrelength) &&
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

vwlineOutline <- function(grob, simplify=TRUE) {
    x <- convertX(grob$x, "in", valueOnly=TRUE)
    y <- convertY(grob$y, "in", valueOnly=TRUE)
    w <- grob$w
    w$left <- pmin(convertWidth(w$left, "in", valueOnly=TRUE),
                   convertHeight(w$left, "in", valueOnly=TRUE))
    w$right <- pmin(convertWidth(w$right, "in", valueOnly=TRUE),
                    convertHeight(w$right, "in", valueOnly=TRUE))
    pts <- vwlinePoints(grob)
    if (grob$open) {
        sinfo <- segInfo(x, y, w, grob$open, grob$stepWidth, grob$debug)
        einfo <- endInfo(x, y, w, sinfo, grob$stepWidth, grob$debug)
        earcinfo <- endArcInfo(sinfo, einfo, grob$debug)
        ends <- buildEnds(w, einfo, earcinfo, grob$stepWidth,
                          grob$lineend, grob$mitrelimit)
        outline <- list(x=c(ends$startx, pts$left$x, ends$endx, pts$right$x),
                        y=c(ends$starty, pts$left$y, ends$endy, pts$right$y))
    } else {
        outline <- list(x=pts$left, y=pts$right)
    }
    xna <- is.na(outline$x)
    yna <- is.na(outline$y)
    if (any(xna | yna)) {
        outline$x <- outline$x[!(xna | yna)]
        outline$y <- outline$y[!(xna | yna)]
        warning("Removed NA values from outline")
    }
    if (simplify)
        polysimplify(outline, filltype="nonzero")
    else
        outline
}

makeContent.vwlineGrob <- function(x, ...) {
    outline <- vwlineOutline(x)
    ## outline is list of outlines
    addGrob(x,
            x$render(unlist(lapply(outline, "[[", "x")),
                     unlist(lapply(outline, "[[", "y")),
                     sapply(outline, function(o) length(o$x)),
                     x$gp, "outline"))
}

edgePoints.vwlineGrob <- function(x, d,
                                  x0, y0,
                                  which=1,
                                  direction="forward",
                                  debug=FALSE,
                                  ...) {
    ## Silently force which to length 1
    which <- which[1]
    outline <- vwlineOutline(x)
    ## outline is list of outlines
    if (which > length(outline)) {
        stop("Invalid which value")
    }
    edge <- outline[[which]]
    if (!is.unit(x0)) x0 <- unit(x0, "npc")
    if (!is.unit(y0)) y0 <- unit(y0, "npc")
    pts <- reorderEdge(edge, x0, y0)
    vwEdgePoints(pts, d, direction == "forward", x$open, debug)
}

outline.vwlineGrob <- function(x, simplify=TRUE, ...) {
    vwlineOutline(x, simplify=simplify)
}
