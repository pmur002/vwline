
library(vwline)

pts <- function(x, y, col="grey") {
    if (length(x)) {
        grid.points(x, y, default.units="in", size=unit(2, "mm"), pch=16,
                    gp=gpar(col=col))
    }
}
lines <- function(x, y, col="grey") {
    if (length(x)) {
        grid.lines(x, y, default.units="in", gp=gpar(col=col))
    }
}

simpleLine <- function(x, y, w) {
    pushViewport(viewport(layout=grid.layout(2, 2)))
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
    pts(x, y)
    lines(x, y)
    grid.vwline(x, y, w, default.units="in", debug=TRUE, gp=gpar(col="black"))
    popViewport()
    pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
    pts(x, y)
    lines(x, y)
    grid.vwline(x, y, w, default.units="in", gp=gpar(col="black"),
                linejoin="round", lineend="round")
    popViewport()
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
    pts(x, y)
    lines(x, y)
    grid.vwline(x, y, w, default.units="in", gp=gpar(col="black"),
                linejoin="mitre", lineend="mitre")
    popViewport()
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
    pts(x, y)
    lines(x, y)
    grid.vwline(x, y, w, default.units="in", gp=gpar(col="black"),
                linejoin="bevel", lineend="square")
    popViewport()
    popViewport()
}

## Non-variable line width (different line endings)
grid.newpage()
scale <- .5
pushViewport(viewport(layout=grid.layout(2, 2)))
x <- scale*c(1, 2)
y <- scale*c(1, 2)
w <- scale*c(1, 1)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
simpleLine(x, y, w)
popViewport()
x <- scale*c(1, 2)
y <- scale*c(2, 1)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
simpleLine(x, y, w)
popViewport()
x <- scale*c(1, 2)
y <- scale*c(1, 1)
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
simpleLine(x, y, w)
popViewport()
x <- scale*c(1, 1)
y <- scale*c(1, 2)
pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
simpleLine(x, y, w)
popViewport()

## Simple line segment horiz/vert round/square/mitre (and mitre-limit)
grid.newpage()
scale <- .5
pushViewport(viewport(layout=grid.layout(2, 2)))
x <- scale*c(1, 2)
y <- scale*c(1, 1)
w <- scale*c(.5, 1)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
simpleLine(x, y, w)
popViewport()
x <- scale*c(2, 1)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
simpleLine(x, y, w)
popViewport()
x <- scale*c(1, 1)
y <- scale*c(1, 2)
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
simpleLine(x, y, w)
popViewport()
y <- scale*c(2, 1)
pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
simpleLine(x, y, w)
popViewport()

## Simple single corner, left width != right width (different line joins/ends)
## end width decreasing
grid.newpage()
scale <- .5
pushViewport(viewport(layout=grid.layout(2, 2)))
x <- scale*c(1, 2, 3)
y <- scale*c(1, 1, 2)
w <- scale*c(.5, 1, .1)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
simpleLine(x, y, list(left=.5*w, right=.25*rev(w)))
popViewport()
x <- scale*c(3, 2, 1)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
simpleLine(x, y, list(left=.5*w, right=.25*rev(w)))
popViewport()
x <- scale*c(1, 1, 2)
y <- scale*c(1, 2, 3)
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
simpleLine(x, y, list(left=.5*w, right=.25*rev(w)))
popViewport()
y <- scale*c(3, 2, 1)
pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
simpleLine(x, y, list(left=.5*w, right=.25*rev(w)))
popViewport()

## Simple single corner, left width != right width (different line joins/ends)
## end width increasing
grid.newpage()
scale <- .5
pushViewport(viewport(layout=grid.layout(2, 2)))
x <- scale*c(1, 2, 3)
y <- scale*c(1, 1, 2)
w <- scale*c(1, .1, .5)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
simpleLine(x, y, list(left=.5*w, right=.25*rev(w)))
popViewport()
x <- scale*c(3, 2, 1)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
simpleLine(x, y, list(left=.5*w, right=.25*rev(w)))
popViewport()
x <- scale*c(1, 1, 2)
y <- scale*c(1, 2, 3)
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
simpleLine(x, y, list(left=.5*w, right=.25*rev(w)))
popViewport()
y <- scale*c(3, 2, 1)
pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
simpleLine(x, y, list(left=.5*w, right=.25*rev(w)))
popViewport()

## Crazy inner corners
grid.newpage()
scale <- .5
pushViewport(viewport(layout=grid.layout(2, 2)))
## Inner edges stick out
x <- scale*c(1, 2, 3)
y <- scale*c(1, 1.5, 1)
w <- scale*c(.2, 2, .2)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
simpleLine(x, y, w)
popViewport()
## One inner edge sticks out
x <- scale*c(1, 2, 3)
y <- scale*c(1, 1.5, 1)
w <- scale*c(.2, 1.5, 1.5)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
simpleLine(x, y, w)
popViewport()
## One inner edge MISSES other
x <- scale*c(1, 2, 3)
y <- scale*c(1, 1.5, 1)
w <- scale*c(.2, 2, 2)
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
simpleLine(x, y, w)
popViewport()
## One inner edge MISSES other (reverse orientation)
x <- scale*rev(c(1, 2, 3))
y <- scale*c(1, 1.5, 1)
w <- scale*c(.2, 2, 2)
pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
simpleLine(x, y, w)
popViewport()

grid.newpage()
scale <- .5
pushViewport(viewport(layout=grid.layout(2, 2)))
## Psycho case
x <- scale*c(1, 1, 1.5)
y <- scale*c(2, 1.5, .5)
w <- scale*c(1, .2, 0)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
simpleLine(x, y, w)
popViewport()
## Square end becomes mitre
x <- scale*c(1, 1.2)
y <- scale*c(1, 1)
w <- scale*c(.3, 1.5)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
simpleLine(x, y, w)
popViewport()
## One outer edge MISSES other 
x <- scale*c(1.5, 2, 3)
y <- scale*c(1, 1.5, 1)
w <- scale*c(2, .5, .5)
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
simpleLine(x, y, w)
popViewport()
## One outer edge MISSES other (reverse orientation)
x <- scale*c(3, 2.5, 1.5)
y <- scale*c(1, 1.5, 1)
w <- scale*c(2, .5, .5)
pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
simpleLine(x, y, w)
popViewport()

## Crazy line
grid.newpage()
scale <- .3
x <- (1 + c(1, 3, 5, 7, 9, 9))*scale
y <- (1 + c(2, 4, 4, 5, 6, 2))*scale
w <- 2*c(.5, 1, .5, .2, .7, 0)*scale
pushViewport(viewport(layout=grid.layout(3, 2)))
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
pts(x, y)
lines(x, y)
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
grid.vwline(x, y, w, default.units="in", debug=TRUE, gp=gpar(col="black"))
popViewport()
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
pts(x, y)
lines(x, y)
grid.vwline(x, y, w, default.units="in", gp=gpar(col="black"),
            linejoin="round", lineend="round")
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
pts(x, y)
lines(x, y)
grid.vwline(x, y, w, default.units="in", gp=gpar(col="black"),
            linejoin="mitre", lineend="mitre")
popViewport()
pushViewport(viewport(layout.pos.row=3, layout.pos.col=2))
pts(x, y)
lines(x, y)
grid.vwline(x, y, w, default.units="in", gp=gpar(col="black"),
            linejoin="bevel", lineend="square")
popViewport()

## Crazy line (stepWidth)
grid.newpage()
scale <- .3
x <- (1 + c(1, 3, 5, 7, 9, 9))*scale
y <- (1 + c(2, 4, 4, 5, 6, 2))*scale
w <- 2*c(.5, 1, .5, .2, .7, 0)*scale
pushViewport(viewport(layout=grid.layout(3, 2)))
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
pts(x, y)
lines(x, y)
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
grid.vwline(x, y, w, default.units="in", stepWidth=TRUE,
            debug=TRUE, gp=gpar(col="black"))
popViewport()
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
pts(x, y)
lines(x, y)
grid.vwline(x, y, w, default.units="in", stepWidth=TRUE, gp=gpar(col="black"),
            linejoin="round", lineend="round")
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
pts(x, y)
lines(x, y)
grid.vwline(x, y, w, default.units="in", stepWidth=TRUE, gp=gpar(col="black"),
            linejoin="mitre", lineend="mitre")
popViewport()
pushViewport(viewport(layout.pos.row=3, layout.pos.col=2))
pts(x, y)
lines(x, y)
grid.vwline(x, y, w, default.units="in", stepWidth=TRUE, gp=gpar(col="black"),
            linejoin="bevel", lineend="square")
popViewport()

## Tests of corners
## 'x' and 'y' should be three values
## 'owidths' should be nine values
testCorners <- function(x, y, leftwidth, midwidth, rightwidth) {
    pushViewport(viewport(layout=grid.layout(3, 3)))
    for (i in 1:3) {
        for (j in 1:3) {
            pushViewport(viewport(layout.pos.row=i, layout.pos.col=j))
            grid.rect(gp=gpar(col="grey"))
            wi <- (i-1)*3 + j
            simpleLine(x, y, c(leftwidth[wi], midwidth[wi], rightwidth[wi]))
            popViewport()
        }
    }
    popViewport()
}
grid.newpage()
scale <- .25
x <- scale*c(1, 2, 3)
y <- scale*c(1, 1, .5)
mw <- scale*rep(.3, 9)
lw <- scale*seq(.1, .9, .1)
rw <- scale*seq(.1, .9, .1)
testCorners(x, y, lw, mw, rw)

grid.newpage()
scale <- .25
x <- scale*c(1, 2, 3)
y <- scale*c(1, 1, .5)
mw <- scale*rep(1.5, 9)
lw <- scale*seq(.1, .9, .1)
rw <- scale*seq(.1, .9, .1)
testCorners(x, y, lw, mw, rw)

grid.newpage()
scale <- .25
x <- scale*c(1, 2, 3)
y <- scale*c(1, 1, .5)
mw <- scale*rep(.3, 9)
lw <- scale*seq(.1, .9, .1)
rw <- scale*seq(.9, .1, -.1)
testCorners(x, y, lw, mw, rw)
