
################################################################################
## Handle spacing specification
resolveDistance <- function(d, length) {
    if (is.unit(d)) {
        dd <- pmin(convertX(d, "in", valueOnly=TRUE),
                   convertY(d, "in", valueOnly=TRUE))
    } else {
        ## Assume s is proportions of length
        dd <- d*length
    }
    dd <- sort(dd)
    ## Repeat to fill path
    mdd <- max(dd)
    if (mdd < length) {
        multiple <- (length - mdd) %/% mdd + 1
        dd <- c(dd, rep(1:multiple*mdd, each=length(dd)) + rep(dd, multiple))
    }
    ## Trim to path 
    rd <- round(dd, 4)
    subset <- rd >= 0 & rd <= round(length, 4)
    rd[subset]
}

