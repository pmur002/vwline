
pdf("edges-tests.pdf", compress=FALSE)

library(vwline)

grid.offsetBezier(c(.2, .4, .6, .8), c(.1, .3, .3, .1),
                  w=c(0, .1, .1, 0), name="b1")
grid.offsetBezier(c(.2, .4, .6, .8), c(.1, .3, .3, .1) + .3,
                  w=widthSpline(c(0, .1, .1, 0)), name="b2")
grid.offsetBezier(c(.2, .4, .6, .8), c(.1, .3, .3, .1) + .6,
                  w=BezierWidth(c(0, .1, .1, 0)), name="b3")

dots <- function(x) {
    pts <- edgePoints(grid.get(x), unit(1:5, "cm"), .2, .1, dir="backwards")
    grid.circle(pts$x, pts$y, r=unit(1, "mm"), gp=gpar(fill=grey(1:5/5)))
}
dots("b1")
dots("b2")
dots("b3")

dev.off()

savedPDF <- system.file("regression-tests", "edges-tests.save.pdf",
                        package="vwline")
diff <- tools::Rdiff("edges-tests.pdf", savedPDF)

if (diff != 0L) {
    ## If differences found, generate images of the differences and error out
    system("pdfseparate edges-tests.pdf edges-test-pages-%d.pdf")
    system(paste0("pdfseparate ", savedPDF, " edges-model-pages-%d.pdf"))
    modelFiles <- list.files(pattern="edges-model-pages-.*")
    N <- length(modelFiles)
    for (i in 1:N) {
        system(paste0("compare edges-model-pages-", i, ".pdf ",
                      "edges-test-pages-", i, ".pdf ",
                      "edges-diff-pages-", i, ".png"))
    } 
    stop("Regression testing detected differences")
}
