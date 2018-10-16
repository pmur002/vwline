
pdf("widths-tests.pdf", compress=FALSE)

library(vwline)

grid.offsetBezier(c(.2, .4, .6, .8), c(.1, .3, .3, .1),
                  w=c(0, .1, .1, 0))
grid.offsetBezier(c(.2, .4, .6, .8), c(.1, .3, .3, .1) + .3,
                  w=widthSpline(c(0, .1, .1, 0)))
grid.offsetBezier(c(.2, .4, .6, .8), c(.1, .3, .3, .1) + .6,
                  w=BezierWidth(c(0, .1, .1, 0)))

dev.off()

savedPDF <- system.file("regression-tests", "widths-tests.save.pdf",
                        package="vwline")
diff <- tools::Rdiff("widths-tests.pdf", savedPDF)

if (diff != 0L) {
    ## If differences found, generate images of the differences and error out
    system("pdfseparate widths-tests.pdf widths-test-pages-%d.pdf")
    system(paste0("pdfseparate ", savedPDF, " widths-model-pages-%d.pdf"))
    modelFiles <- list.files(pattern="widths-model-pages-.*")
    N <- length(modelFiles)
    for (i in 1:N) {
        system(paste0("compare widths-model-pages-", i, ".pdf ",
                      "widths-test-pages-", i, ".pdf ",
                      "widths-diff-pages-", i, ".png"))
    } 
    stop("Regression testing detected differences")
}
