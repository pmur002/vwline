## For debugging diagrams
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
segs <- function(x1, y1, x2, y2, col="grey") {
    if (length(x1)) {
        grid.segments(x1, y1, x2, y2, default.units="in", gp=gpar(col=col))
    }
}
polyl <- function(x, y, id=length(x), col="black") {
    if (length(x)) {    
        grid.polyline(x, y, default.units="in", id=id, gp=gpar(col=col))
    }
}
polyg <- function(x, y, id=rep(1, length(x)), col="black", fill=NA) {
    if (length(x)) {
        grid.polygon(x, y, default.units="in", id=id,
                     gp=gpar(col=col, fill=fill))
    }
}
