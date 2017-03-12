
library(vwline)

vwx <- vwXsplineGrob(c(.2, .5, .8), c(.2, .2, .8), unit(c(0, 10, 20), "mm"))

grid.newpage()
grid.draw(vwx)
pts <- vwline:::vwXsplinePoints(vwx)
grid.points(pts$left$x, pts$left$y,
            size=unit(rep(2, length(pts$left$x)), "mm"),
            pch=16, gp=gpar(col="red"))

vbx <- brushXsplineGrob(verticalBrush,
                       c(.2, .5, .8), c(.2, .2, .8),
                       unit(20, "mm"),
                       gp=gpar(col="black"))
grid.newpage()
grid.draw(vbx)
pts <- vwline:::brushXsplineOutline(vbx)
grid.points(pts[[1]]$x[1:20], pts[[1]]$y[1:20], default.units="in",
            size=unit(rep(2, 10), "mm"),
            pch=16, gp=gpar(col="green"))
N <- length(pts[[1]]$x)
grid.points(pts[[1]]$x[(N-20):N], pts[[1]]$y[(N-20):N], default.units="in",
            size=unit(rep(2, 10), "mm"),
            pch=16, gp=gpar(col="red"))

vbx2 <- editGrob(vbx, x=rev(vbx$x), y=rev(vbx$y),
                 gp=gpar(col=rgb(0,0,1,.5), lwd=5))
grid.draw(vbx2)
pts <- vwline:::brushXsplineOutline(vbx2)
grid.points(pts[[1]]$x[1:20], pts[[1]]$y[1:20], default.units="in",
            size=unit(rep(4, 10), "mm"),
            pch=16, gp=gpar(col=rgb(0,1,1,.5)))
N <- length(pts[[1]]$x)
grid.points(pts[[1]]$x[(N-20):N], pts[[1]]$y[(N-20):N], default.units="in",
            size=unit(rep(4, 10), "mm"),
            pch=16, gp=gpar(col=rgb(1,0,1,.5)))

cbx <- brushXsplineGrob(circleBrush(),
                       c(.2, .5, .8), c(.2, .2, .8),
                       unit(20, "mm"),
                       gp=gpar(col="black"))
grid.newpage()
grid.draw(cbx)
pts <- vwline:::brushXsplineOutline(cbx)
grid.points(pts[[1]]$x[1:20], pts[[1]]$y[1:20], default.units="in",
            size=unit(rep(2, 10), "mm"),
            pch=16, gp=gpar(col="green"))
N <- length(pts[[1]]$x)
grid.points(pts[[1]]$x[(N-20):N], pts[[1]]$y[(N-20):N], default.units="in",
            size=unit(rep(2, 10), "mm"),
            pch=16, gp=gpar(col="red"))

sbx <- brushXsplineGrob(squareBrush,
                       c(.2, .5, .8), c(.2, .2, .8),
                       unit(20, "mm"),
                       gp=gpar(col="black"))
grid.newpage()
grid.draw(sbx)
pts <- vwline:::brushXsplineOutline(sbx)
grid.points(pts[[1]]$x[1:20], pts[[1]]$y[1:20], default.units="in",
            size=unit(rep(2, 10), "mm"),
            pch=16, gp=gpar(col="green"))
N <- length(pts[[1]]$x)
grid.points(pts[[1]]$x[(N-20):N], pts[[1]]$y[(N-20):N], default.units="in",
            size=unit(rep(2, 10), "mm"),
            pch=16, gp=gpar(col="red"))

pts1 <- vwline:::placeBrush(circleBrush(), 1/3, .5, size=.5)
pts2 <- vwline:::placeBrush(circleBrush(), 2/3, .5, size=.5)
grid.newpage()
grid.polygon(pts1$x, pts1$y)
grid.polygon(pts2$x, pts2$y)
grid.points(pts1$x[1:10], pts1$y[1:10], default.units="npc",
            size=unit(rep(2, 10), "mm"),
            pch=16, gp=gpar(col="red"))
grid.points(pts2$x[1:10], pts2$y[1:10], default.units="npc",
            size=unit(rep(2, 10), "mm"),
            pch=16, gp=gpar(col="red"))
pts3 <- vwline:::combineShapes(pts1, pts2)[[1]]
grid.polygon(pts3$x, pts3$y, gp=gpar(col=rgb(0,0,1,.5), lwd=5))
grid.points(pts3$x[1:10], pts3$y[1:10], default.units="npc",
            size=unit(rep(2, 10), "mm"),
            pch=16, gp=gpar(col=hcl(120, 80, seq(100, 60, length=10))))

reorderBorder <- function(pts, x0, y0, clockwise=FALSE) {
    if (clockwise) {
        pts <- list(x=rev(pts$x), y=rev(pts$y))
    }
    N <- length(pts$x)
    dist <- (pts$x - x0)^2 + (pts$y - y0)^2
    closest <- which.min(dist)[1]
    order <- (1:N + closest - 2) %% N
    list(x=pts$x[order], y=pts$y[order])
}

pts4 <- reorderBorder(pts3, 0, 0)
grid.points(pts4$x[1:10], pts4$y[1:10], default.units="npc",
            size=unit(rep(2, 10), "mm"),
            pch=16, gp=gpar(col=hcl(120, 80, seq(100, 60, length=10))))

pts5 <- reorderBorder(pts3, 0, 0.5)
grid.points(pts5$x[1:10], pts5$y[1:10], default.units="npc",
            size=unit(rep(2, 10), "mm"),
            pch=16, gp=gpar(col=hcl(120, 80, seq(100, 60, length=10))))

pts6 <- reorderBorder(pts3, 0, 0.5, clockwise=TRUE)
grid.points(pts6$x[1:10], pts6$y[1:10], default.units="npc",
            size=unit(rep(2, 10), "mm"),
            pch=16, gp=gpar(col=hcl(120, 80, seq(100, 60, length=10))))

pts7 <- reorderBorder(pts3, 1, 0.5)
grid.points(pts7$x[1:10], pts7$y[1:10], default.units="npc",
            size=unit(rep(2, 10), "mm"),
            pch=16, gp=gpar(col=hcl(120, 80, seq(100, 60, length=10))))
