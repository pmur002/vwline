
pdf("endsjoins-tests.pdf", compress=FALSE)

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
    grid.vwline(x, y, w, default.units="in",
                gp=gpar(col="black", fill=rgb(0,0,0,.2)),
                linejoin="round", lineend="round")
    popViewport()
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
    pts(x, y)
    lines(x, y)
    grid.vwline(x, y, w, default.units="in",
                gp=gpar(col="black", fill=rgb(0,0,0,.2)),
                linejoin="mitre", lineend="mitre")
    popViewport()
    pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
    pts(x, y)
    lines(x, y)
    grid.vwline(x, y, w, default.units="in", 
                gp=gpar(col="black", fill=rgb(0,0,0,.2)),
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
grid.vwline(x, y, w, default.units="in",
            gp=gpar(col="black", fill=rgb(0,0,0,.2)),
            linejoin="round", lineend="round")
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
pts(x, y)
lines(x, y)
grid.vwline(x, y, w, default.units="in", 
            gp=gpar(col="black", fill=rgb(0,0,0,.2)),
            linejoin="mitre", lineend="mitre")
popViewport()
pushViewport(viewport(layout.pos.row=3, layout.pos.col=2))
pts(x, y)
lines(x, y)
grid.vwline(x, y, w, default.units="in", 
            gp=gpar(col="black", fill=rgb(0,0,0,.2)),
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
grid.vwline(x, y, w, default.units="in", stepWidth=TRUE, 
            gp=gpar(col="black", fill=rgb(0,0,0,.2)),
            linejoin="round", lineend="round")
popViewport()
pushViewport(viewport(layout.pos.row=2, layout.pos.col=2))
pts(x, y)
lines(x, y)
grid.vwline(x, y, w, default.units="in", stepWidth=TRUE, 
            gp=gpar(col="black", fill=rgb(0,0,0,.2)),
            linejoin="mitre", lineend="mitre")
popViewport()
pushViewport(viewport(layout.pos.row=3, layout.pos.col=2))
pts(x, y)
lines(x, y)
grid.vwline(x, y, w, default.units="in", stepWidth=TRUE, 
            gp=gpar(col="black", fill=rgb(0,0,0,.2)),
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

## Test mitre join becoming bevel join
grid.newpage()
x <- c(.1, .15, .2)
y <- c(.4, .5, .4)
w <- unit(rep(5, 3), "mm")
grid.vwline(x, y, w, linejoin="mitre")
grid.text("within mitre limit (4)", x=.15, y=.55, just="left", rot=90)
x <- c(.3, .35, .4)
y <- c(.3, .7, .3)
grid.vwline(x, y, w, linejoin="mitre")
grid.text("exceeds mitre limit (4)", x=.35, y=.25)
x <- c(.5, .55, .6)
y <- c(.4, .5, .4)
grid.vwline(x, y, w, linejoin="mitre", mitrelimit=1)
grid.text("exceeds mitre limit (1)", x=.55, y=.55, just="left", rot=90)
x <- c(.7, .75, .8)
y <- c(.3, .7, .3)
grid.vwline(x, y, w, linejoin="mitre", mitrelimit=10)
grid.text("within mitre limit (10)", x=.75, y=.25)

## Tests for sharp corner, large change in width, stepWidth=TRUE
## 'len' is length of second segment
## 'width' is width of second segment
drawLine <- function(len, width, debug=FALSE, rev=FALSE) {
    x <- c(.2, .6, .6)
    y <- c(.2, .8, .8 - len)
    w <- c(.1, width, 0)
    if (rev) {
        x <- rev(x)
        y <- rev(y)
        w <- c(width, .1, 0)
    }
    if (debug) {
        grid.vwline(x, y, w, stepWidth=TRUE, debug=TRUE, linejoin="mitre",
                    gp=gpar(col="black"))
    } else {
        grid.vwline(x, y, w, stepWidth=TRUE, linejoin="mitre",
                    gp=gpar(col="black", fill=rgb(0,0,0,.2)))
    }
}

drawPage <- function(params, debug=FALSE, rev=FALSE) {
    N <- nrow(params)
    dims <- n2mfrow(N)
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(dims[1], dims[2], respect=TRUE)))
    for (i in 1:N) {
        row <- (i - 1) %/% dims[2] + 1
        col <- (i - 1) %% dims[2] + 1
        pushViewport(viewport(layout.pos.col=col, layout.pos.row=row))
        drawLine(params[i,1], params[i,2], debug, rev)
        popViewport()
    }
}
    
params <- rbind(c(.03, .05), 
                c(.1, .05), # rt edge of 2nd seg entirely within 1st seg
                c(.1, .1), # rt edge of 2nd seg entirely within 1st seg
                c(.1, .4), # rt edge of 2nd seg entirely wrong side of 1st seg
                c(.3, .4), # rt edge of 2nd seg ends within 1st seg
                c(.4, .4), # rt edge of 2nd seg ends within 1st seg
                c(.7, .8), # rt edge of 2nd seg intersects start end of 1st seg
                c(.7, .9), # rt edge of 1st seg entirely within 2nd seg
                ## (sane case)
                c(.4, .1) # rt edge of 2nd seg intersects rt edge of 1st seg
                )

drawPage(params, debug=TRUE)
drawPage(params, debug=FALSE)
drawPage(params, debug=TRUE, rev=TRUE)
drawPage(params, debug=FALSE, rev=TRUE)

grid.newpage()
## offsetBezier corners
grid.offsetBezier(c(1:7/8), c(.1, .2, .2, .1, .2, .2, .1),
                  w=unit(1:7/8, "cm"),
                  lineend="round")
## NOTE that the mitre ends exceed the mitrelimit (so are just square)
grid.offsetBezier(c(1:7/8), c(.1, .2, .2, .1, .2, .2, .1) + .2,
                  w=unit(1:7/8, "cm"),
                  linejoin="mitre",
                  lineend="mitre")
grid.offsetBezier(c(1:7/8), c(.1, .2, .2, .1, .2, .2, .1) + .4,
                  w=unit(1:7/8, "cm"),
                  linejoin="bevel",
                  lineend="square")

library(vwline)
grid.newpage()
## MitreLimit will "blunt" extensions
grid.offsetBezier(c(.1, .15, .3, .4, .5, .65, .7),
                  c(.1, .2, .2, .1, .2, .2, .1) + .2,
                  w=BezierWidth(unit(1:7/8, "cm")),
                  linejoin="extend",
                  lineend="extend",
                  gp=gpar(col="black"))
grid.offsetBezier(c(.1, .15, .3, .4, .5, .65, .7),
                  c(.1, .2, .2, .1, .2, .2, .1) + .2,
                  w=BezierWidth(unit(1:7/8, "cm")),
                  linejoin="mitre",
                  lineend="mitre",
                  gp=gpar(col=NA, fill=rgb(0,0,0,.2)))
## Curl at join is such that outside edges do not meet
grid.offsetBezier(c(.1, .15, .35, .4, .45, .65, .7),
                  c(.1, .2, .2, .1, .2, .2, .1) + .6,
                  w=BezierWidth(unit(1:7/8, "cm")),
                  linejoin="extend",
                  lineend="extend",
                  gp=gpar(col="black"))
grid.offsetBezier(c(.1, .15, .35, .4, .45, .65, .7),
                  c(.1, .2, .2, .1, .2, .2, .1) + .6,
                  w=BezierWidth(unit(1:7/8, "cm")),
                  linejoin="mitre",
                  lineend="mitre",
                  gp=gpar(col=NA, fill=rgb(0,0,0,.2)))
grid.newpage()
## Relax mitrelimit
grid.offsetBezier(c(.1, .15, .3, .4, .5, .65, .7),
                  c(.1, .2, .2, .1, .2, .2, .1) + .2,
                  w=BezierWidth(unit(1:7/8, "cm")),
                  linejoin="extend",
                  lineend="extend",
                  mitrelimit=40,
                  gp=gpar(col="black"))
grid.offsetBezier(c(.1, .15, .3, .4, .5, .65, .7),
                  c(.1, .2, .2, .1, .2, .2, .1) + .2,
                  w=BezierWidth(unit(1:7/8, "cm")),
                  linejoin="mitre",
                  lineend="mitre",
                  mitrelimit=30,
                  gp=gpar(col=NA, fill=rgb(0,0,0,.2)))
## Curl at join is such that outside edges do not meet
grid.offsetBezier(c(.1, .15, .35, .4, .45, .65, .7),
                  c(.1, .2, .2, .1, .2, .2, .1) + .6,
                  w=BezierWidth(unit(1:7/8, "cm")),
                  linejoin="extend",
                  lineend="extend",
                  mitrelimit=40,
                  gp=gpar(col="black"))
grid.offsetBezier(c(.1, .15, .35, .4, .45, .65, .7),
                  c(.1, .2, .2, .1, .2, .2, .1) + .6,
                  w=BezierWidth(unit(1:7/8, "cm")),
                  linejoin="mitre",
                  lineend="mitre",
                  mitrelimit=30,
                  gp=gpar(col=NA, fill=rgb(0,0,0,.2)))

dev.off()

savedPDF <- system.file("regression-tests", "endsjoins-tests.save.pdf",
                        package="vwline")
diff <- tools::Rdiff("endsjoins-tests.pdf", savedPDF)

if (diff != 0L) {
    ## If differences found, generate images of the differences and error out
    system("pdfseparate endsjoins-tests.pdf endsjoins-test-pages-%d.pdf")
    system(paste0("pdfseparate ", savedPDF, " endsjoins-model-pages-%d.pdf"))
    modelFiles <- list.files(pattern="endsjoins-model-pages-.*")
    N <- length(modelFiles)
    for (i in 1:N) {
        system(paste0("compare endsjoins-model-pages-", i, ".pdf ",
                      "endsjoins-test-pages-", i, ".pdf ",
                      "endsjoins-diff-pages-", i, ".png"))
    } 
    stop("Regression testing detected differences")
}
