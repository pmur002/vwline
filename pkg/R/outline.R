
## Generic interface for returning points that make up the boundary
## of a curve

## Different from edgePoints() (and simpler) because it returns all points
## on outline
## AND it will allow "unsimplified" outline

## The result is a list of (x, y) lists

## 'x' is the curve object
## 'simplify' says whether to simplify
outline <- function(x, simplify=TRUE, ...) {
    UseMethod("outline")
}
