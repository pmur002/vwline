
################################################################################
## Functions for rendering vw* object outlines

vwPolygon <- function(x, y, id.lengths, gp, name) {
    polygonGrob(x, y, default.units="in",
                id.lengths=id.lengths, gp=gp, name=name)
}

vwPath <- function(rule="winding") {
    function(x, y, id.lengths, gp, name) {
        pathGrob(x, y, default.units="in",
                 id.lengths=id.lengths, rule=rule, gp=gp, name=name)
    }
}
