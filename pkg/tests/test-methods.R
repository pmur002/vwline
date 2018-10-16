
pdf("methods-tests.pdf", compress=FALSE)

## Generate a variety of variable-width shapes with different methods

library(vwline)

N <- 10
    
grid.newpage()
heights <- unit(rep(1, N+1), c("lines", rep("null", N)))
pushViewport(viewport(layout=grid.layout(N+1, 6, heights=heights)))
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1),
             viewport(width=.8, height=.8))
grid.text("vwcurve", gp=gpar(fontfamily="mono", cex=.7))
popViewport(2)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2),
             viewport(width=.8, height=.8))
grid.text("vwXspline", gp=gpar(fontfamily="mono", cex=.7))
popViewport(2)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=3),
             viewport(width=.8, height=.8))
grid.text("vwline", gp=gpar(fontfamily="mono", cex=.7))
popViewport(2)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=4),
             viewport(width=.8, height=.8))
grid.text("brushXspline", gp=gpar(fontfamily="mono", cex=.7))
popViewport(2)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=5),
             viewport(width=.8, height=.8))
grid.text("offsetXspline", gp=gpar(fontfamily="mono", cex=.7))
popViewport(2)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=6),
             viewport(width=.8, height=.8))
grid.text("offsetBezier", gp=gpar(fontfamily="mono", cex=.7))
popViewport(2)

testLine <- function(x, y, w, row,
                     ...,
                     vwcurveArgs=list(),
                     vwXsplineArgs=list(),
                     vwlineArgs=list(),
                     brushXsplineArgs=list(brush=verticalBrush),
                     offsetXsplineArgs=list(),
                     offsetBezierArgs=list()) {
    pushViewport(viewport(layout.pos.row=row, layout.pos.col=1),
                 viewport(width=.8, height=.8))
    do.call(grid.vwcurve,
            c(list(x=x, y=y, w=w), vwcurveArgs, list(...)))
    popViewport(2)
    pushViewport(viewport(layout.pos.row=row, layout.pos.col=2),
                 viewport(width=.8, height=.8))
    do.call(grid.vwXspline,
            c(list(x=x, y=y, w=w), vwXsplineArgs, list(...)))
    popViewport(2)
    pushViewport(viewport(layout.pos.row=row, layout.pos.col=3),
                 viewport(width=.8, height=.8))
    do.call(grid.vwline,
            c(list(x=x, y=y, w=w), vwlineArgs, list(...)))
    popViewport(2)
    pushViewport(viewport(layout.pos.row=row, layout.pos.col=4),
                 viewport(width=.8, height=.8))
    ## brushXspline ignores 'lineend'
    dotargs <- list(...)
    dotargs$lineend <- NULL
    do.call(grid.brushXspline,
            c(list(x=x, y=y, w=w), brushXsplineArgs, dotargs))
    popViewport(2)
    pushViewport(viewport(layout.pos.row=row, layout.pos.col=5),
                 viewport(width=.8, height=.8))
    do.call(grid.offsetXspline,
            c(list(x=x, y=y, w=w), offsetXsplineArgs, list(...)))
    popViewport(2)
    pushViewport(viewport(layout.pos.row=row, layout.pos.col=6),
                 viewport(width=.8, height=.8))
    do.call(grid.offsetBezier,
            c(list(x=x, y=y, w=w), offsetBezierArgs, list(...)))
    popViewport(2)
}

testLine(0:3/3, rep(.5, 4), unit(rep(1, 4), "mm"), row=2)
testLine(0:3/3, rep(.5, 4), unit(1:4, "mm"), row=3)
testLine(c(0, 0, 1, 1, .6, .4), c(.2, 1, 1, .2, 0, 0),
         unit(c(1:3, 3:1), "mm"), open=FALSE, row=4)
testLine(0:3/3, c(.8, .2, .8, .2), unit(1:4, "mm"), row=5, lineend="round",
         brushXsplineArgs=list(brush=circleBrush()))

dev.off()

savedPDF <- system.file("regression-tests", "methods-tests.save.pdf",
                        package="vwline")
diff <- tools::Rdiff("methods-tests.pdf", savedPDF)

if (diff != 0L) {
    ## If differences found, generate images of the differences and error out
    system("pdfseparate methods-tests.pdf methods-test-pages-%d.pdf")
    system(paste0("pdfseparate ", savedPDF, " methods-model-pages-%d.pdf"))
    modelFiles <- list.files(pattern="methods-model-pages-.*")
    N <- length(modelFiles)
    for (i in 1:N) {
        system(paste0("compare methods-model-pages-", i, ".pdf ",
                      "methods-test-pages-", i, ".pdf ",
                      "methods-diff-pages-", i, ".png"))
    } 
    stop("Regression testing detected differences")
}
