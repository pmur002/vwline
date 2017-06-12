fblend <- function(num, denom) {
    u <- num/denom
    p <- 2*denom*denom
    u*u*u*(10 - p + (2*p - 15)*u + (6 - p)*u*u)
}
gblend <- function(u, q) {
    u*(q + u*(2*q + u*(8 - 12*q + u*(14*q - 11 + u*(4 - 5*q)))))
}
hblend <- function(u, q) {
    u*(q + u*(2*q + u*u*(-2*q - u*q)))
}
xsplinePts <- function(px, py, s1, s2, t) {
    if (s1 < 0) {
        A0 <- hblend(-t, -s1)
        A2 <- gblend(t, -s1)
    } else {
        A0 <- ifelse(t < s1, fblend(t - s1, -1 - s1), 0)
        A2 <- fblend(t + s1, 1 + s1)
    }
    if (s2 < 0) {
        A1 <- gblend(1 - t, -s2)
        A3 <- hblend(t - 1, -s2)
    } else {
        A1 <- fblend(t - 1 - s2, -1 - s2)
        A3 <- ifelse(t > 1 - s2, fblend(t - 1 + s2, 1 + s2), 0)
    }
    Asum <- A0 + A1 + A2 + A3
    list(x=(A0*px[1] + A1*px[2] + A2*px[3] + A3*px[4])/Asum,
         y=(A0*py[1] + A1*py[2] + A2*py[3] + A3*py[4])/Asum)
}

xsplineOffsets <- function(px, py, s1, s2, t) {

    evalTangents <- function(i, j) {
        for (k in c("A0", "noA0")) {
            for (m in c("A3", "noA3")) {
                assign(paste("utx", i, j, k, m, sep="."),
                       do.call(get(paste("xsplineTangent",
                                         i, j, k, m, "x", sep=".")),
                               list(px0=px[1], px1=px[2], px2=px[3], px3=px[4],
                                    py0=py[1], py1=py[2], py2=py[3], py3=py[4],
                                    s1=s1, s2=s2, t=t)))
                assign(paste("uty", i, j, k, m, sep="."),
                       do.call(get(paste("xsplineTangent",
                                         i, j, k, m, "y", sep=".")),
                               list(px0=px[1], px1=px[2], px2=px[3], px3=px[4],
                                    py0=py[1], py1=py[2], py2=py[3], py3=py[4],
                                    s1=s1, s2=s2, t=t)))
            }
        }
        list(x=ifelse(t < s1 & t > 1 - s2,
                      get(paste("utx", i, j, "A0.A3", sep=".")),
                      ifelse(t < s1,
                             get(paste("utx", i, j, "A0.noA3", sep=".")),
                             ifelse(t > 1 - s2,
                                    get(paste("utx", i, j, "noA0.A3", sep=".")),
                                    get(paste("utx", i, j, "noA0.noA3",
                                              sep="."))))),
             y=ifelse(t < s1 & t > 1 - s2,
                      get(paste("uty", i, j, "A0.A3", sep=".")),
                      ifelse(t < s1,
                             get(paste("uty", i, j, "A0.noA3", sep=".")),
                             ifelse(t > 1 - s2,
                                    get(paste("uty", i, j, "noA0.A3", sep=".")),
                                    get(paste("uty", i, j, "noA0.noA3",
                                              sep="."))))))
    }
 
    if (s1 < 0) {
        if (s2 < 0) {
            tangents <- evalTangents("s1neg", "s2neg")
        } else {
            tangents <- evalTangents("s1neg", "s2pos")
        }
    } else {
        if (s2 < 0) {
            tangents <- evalTangents("s1pos", "s2neg")
        } else {
            tangents <- evalTangents("s1pos", "s2pos")
        }
    }
    ## Unit normals from unit tangents
    tangentLengths <- sqrt(tangents$x^2 + tangents$y^2)
    unitTangents <- list(x=tangents$x/tangentLengths,
                         y=tangents$y/tangentLengths)
    list(x=unitTangents$y, y=-unitTangents$x)
}

xspline <- function(x, y, shape=1, open=TRUE, repEnds=TRUE,
                    t=seq(0, 1, len=30),
                    xsplineFun=xsplinePts) {
    N <- length(x)
    shape <- rep(shape, length=N)
    if (open) {
        if (is.character(repEnds) && repEnds == "extend") {
            ## Extend first and last control points in direction
            ## of first and last line-between-control-points
            a1 <- angle(x[2:1], y[2:1])
            d1 <- dist(diff(x[2:1]), diff(y[2:1]))
            ext1 <- extend(x[1], y[1], a1, d1)
            a2 <- angle(x[(N-1):N], y[(N-1):N])
            d2 <- dist(diff(x[(N-1):N]), diff(y[(N-1):N]))
            ext2 <- extend(x[N], y[N], a2, d2)
            x <- c(ext1$x, x, ext2$x)
            y <- c(ext1$y, y, ext2$y)
            shape <- c(0, shape, 0)
            N <- N + 2            
        } else if (repEnds) {
            ## Force first and last shape to be zero
            shape[1] <- shape[N] <- 0
            ## Repeat first and last control points
            x <- c(x[1], x, x[N])
            y <- c(y[1], y, y[N])
            shape <- c(shape[1], shape, shape[N])
            N <- N + 2
        }
    } else if (!open) {
        ## Add last control point to start
        ## AND last first two control points to end
        x <- c(x[N], x, x[1:2])
        y <- c(y[N], y, y[1:2])
        shape <- c(shape[N], shape, shape[1:2])
        N <- N + 3
    }
    curves <- vector("list", N)
    for (i in 1:(N-3)) {
        index <- i:(i+3)
        curves[[i]] <- xsplineFun(x[index], y[index],
                                  shape[i+1], shape[i+2], t)
    }
    cx <- unlist(lapply(curves, "[[", "x"))
    cy <- unlist(lapply(curves, "[[", "y"))
    list(x=cx, y=cy)
}
