
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
    ## Trim to path 
    rs <- round(ss, 4)
    subset <- rs >= 0 & rs <= round(length, 4)
    ss[subset]
}

