
################################################################################
## Handle specification of distance along line
resolveDistance <- function(d, length, fill=TRUE) {
    if (is.unit(d)) {
        dd <- pmin(convertX(d, "in", valueOnly=TRUE),
                   convertY(d, "in", valueOnly=TRUE))
    } else {
        ## Assume s is proportions of length
        dd <- d*length
    }
    dd <- sort(dd)
    if (fill) {
        ## Repeat to fill path
        mdd <- max(dd)
        if (mdd < length) {
            multiple <- (length - mdd) %/% mdd + 1
            dd <- c(dd,
                    rep(1:multiple*mdd, each=length(dd)) + rep(dd, multiple))
        }
    }
    ## Trim to path 
    rd <- round(dd, 4)
    subset <- rd >= 0 & rd <= round(length, 4)
    ## Make sure that locations at either end of the line are included
    dstart <- rd == 0
    if (any(dstart)) {
        dd[dstart] <- 0
    }
    dend <- rd == round(length, 4)
    if (any(dend)) {
        dd[dend] <- length
    }
    dd[subset]
}

