
################################################################################
## Handle spacing specification
resolveSpacing <- function(s, length) {
    if (is.unit(s)) {
        ss <- pmin(convertX(s, "in", valueOnly=TRUE),
                   convertY(s, "in", valueOnly=TRUE))
    } else {
        ## Assume s is proportions of length
        ss <- s*length
    }
    ss <- sort(ss)
    ## Repeat to fill path
    mss <- max(ss)
    if (mss < length) {
        multiple <- (length - mss) %/% mss + 1
        ss <- c(ss, rep(1:multiple*mss, each=length(ss)) + rep(ss, multiple))
    }
    ## Trim to path 
    rs <- round(ss, 4)
    subset <- rs >= 0 & rs <= round(length, 4)
    rs[subset]
}

