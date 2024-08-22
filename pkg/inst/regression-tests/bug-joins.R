
library(vwline)

pdf("bug-joins.pdf", compress=FALSE)

## The bug was that the last corner on a closed line was not being added
## (was reduced to bevel).
## For some orderings of vertices, this also produced weird artifacts.

## make a line with a bend
x <- c(.4, .6, .6)
y <- c(.4, .4, .6)

## define fixed line width
w <- rep(0.04, length(x));

## Bevel corners
grid.newpage()
grid.vwline(x=x, y=y, w=w,
   linejoin="bevel",
   open=FALSE, gp=grid::gpar(col="black", fill="darkorange"))

## Round corners
grid.newpage()
grid.vwline(x=x, y=y, w=w,
   linejoin="round",
   open=FALSE, gp=grid::gpar(col="black", fill="darkorange"))

## Reverse order of vertices (round corners)
x2 <- rev(x);
y2 <- rev(y);
grid.newpage()
grid.vwline(x=x2, y=y2, w=w,
   linejoin="round",
   open=FALSE, gp=grid::gpar(col="black", fill="darkorange"))

## Open version(s)
grid.newpage()
grid.vwline(x=x, y=y, w=w,
   linejoin="round",
   open=TRUE, gp=grid::gpar(col="black", fill="darkorange"))

grid.newpage()
grid.vwline(x=x2, y=y2, w=w,
   linejoin="round",
   open=TRUE, gp=grid::gpar(col="black", fill="darkorange"))

## Mitre corners
grid.newpage()
grid.vwline(x=x, y=y, w=w,
   linejoin="mitre",
   open=FALSE, gp=grid::gpar(col="black", fill="darkorange"))

dev.off()

savedPDF <- system.file("regression-tests", "bug-joins.save.pdf",
                        package="vwline")
diff <- tools::Rdiff("bug-joins.pdf", savedPDF)

if (diff != 0L) {
    ## If differences found, generate images of the differences and error out
    system("pdfseparate bug-joins.pdf bug-joins-test-pages-%d.pdf")
    system(paste0("pdfseparate ", savedPDF, " bug-joins-model-pages-%d.pdf"))
    modelFiles <- list.files(pattern="bug-joins-model-pages-.*")
    N <- length(modelFiles)
    for (i in 1:N) {
        system(paste0("compare bug-joins-model-pages-", i, ".pdf ",
                      "bug-joins-test-pages-", i, ".pdf ",
                      "bug-joins-diff-pages-", i, ".png"))
    } 
    stop("Regression testing detected differences")
}
