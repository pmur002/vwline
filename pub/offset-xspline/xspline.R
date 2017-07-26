
## For symbolic differentiation of expressions
library(Ryacas)

## Same calculation in single (long) expression
## (string macro expansion)
xsplineFunGenerator <- function(s1, s2, A0, A3) {
    ## Evaluate arguments so can call this function within a loop
    force(s1)
    force(s2)
    force(A0)
    force(A3)
    function(xy) {
        fblend1 <- "u*u*u*(10 - p + (2*p - 15)*u + (6 - p)*u*u)"
        fblend2 <- gsub("p", "(2*denom*denom)",
                        gsub("u", "(num/denom)", fblend1))
        gblend <- "u*(q + u*(2*q + u*(8 - 12*q + u*(14*q - 11 + u*(4 - 5*q)))))"
        hblend <- "u*(q + u*(2*q + u*u*(-2*q - u*q)))"
        if (s1 == "s1neg") {
            A0 <- gsub("u", "(-t)",
                       gsub("q", "(-s1)", hblend))
        } else {
            if (A0 == "A0") {
                A0 <- gsub("num", "(t - s1)",
                           gsub("denom", "(-1 - s1)", fblend2))
            } else {
                A0 <- 0
            }
        }
        if (s2 == "s2neg") {
            A1 <- gsub("u", "(1 - t)",
                       gsub("q", "(-s2)", gblend))       
        } else {
            A1 <- gsub("num", "(t - 1 - s2)",
                       gsub("denom", "(-1 - s2)", fblend2))
        }
        if (s1 == "s1neg") {
            A2 <- gsub("u", "t",
                       gsub("q", "(-s1)", gblend))
        } else {
            A2 <- gsub("num", "(t + s1)",
                       gsub("denom", "(1 + s1)", fblend2))
        }
        if (s2 == "s2neg") {
            A3 <- gsub("u", "(t - 1)",
                       gsub("q", "(-s2)", hblend))
        } else {
            if (A3 == "A3") {
                A3 <- gsub("num", "(t - 1 + s2)",
                           gsub("denom", "(1 + s2)", fblend2))
            } else {
                A3 <- 0
            }
        }
        points <- paste0("(A0*p", xy, "0 +",
                         " A1*p", xy, "1 +",
                         " A2*p", xy, "2 +",
                         " A3*p", xy, "3)/(A0 + A1 + A2 + A3)")
        expr <- gsub("A0", A0,
                     gsub("A1", A1,
                          gsub("A2", A2,
                               gsub("A3", A3, points))))
        parse(text=expr)
    }
}

notrun <- function() {
for (i in c("s1pos", "s1neg")) {
    for (j in c("s2pos", "s2neg")) {
        for (k in c("A0", "noA0")) {
            for (m in c("A3", "noA3")) {
                fname <- paste("xsplineFun", i, j, k, m, sep=".")
                assign(fname, xsplineFunGenerator(i, j, k, m))
                fx <- get(fname)("x")
                fy <- get(fname)("y")
                dput(fx, paste0("Expressions/", fname, ".x.R"), control=NULL)
                dput(fy, paste0("Expressions/", fname, ".y.R"), control=NULL)
            }
        }
    }
}
}

xsplineTangentExpr <- function(FUN) {
    fx <- FUN("x")
    dfx <- D(fx, "t")
    fy <- FUN("y")
    dfy <- D(fy, "t")
    list(x=dfx, y=dfy)
}

notrun <- function() {
for (i in c("s1pos", "s1neg")) {
    for (j in c("s2pos", "s2neg")) {
        for (k in c("A0", "noA0")) {
            for (m in c("A3", "noA3")) {
                utname <- paste("xsplineTangent", i, j, k, m, sep=".")
                assign(utname,
                       xsplineTangentExpr(get(paste("xsplineFun",
                                                    i, j, k, m,
                                                    sep="."))))
                dput(as.expression(get(utname)$x),
                     file=paste0("Expressions/", utname, ".x.R"))
                dput(as.expression(get(utname)$y),
                     file=paste0("Expressions/", utname, ".y.R"))
            }
        }
    }
}
}

## Convert dput()ed expressions to R function definitions
notrun <- function() {
for (i in c("s1pos", "s1neg")) {
    for (j in c("s2pos", "s2neg")) {
        for (k in c("A0", "noA0")) {
            for (m in c("A3", "noA3")) {
                fname <- paste("xsplineTangent", i, j, k, m, sep=".")
                fxname <- paste0("Expressions/", fname, ".x.R")
                xcode <- readLines(fxname)
                writeLines(c(gsub("expression",
                                  paste0(fname, ".x ",
                                         "<- function",
                                         "(px0, px1, px2, px3, py0, py1, py2, py3, s1, s2, t) ",
                                         "{\n"),
                                  xcode),
                             "}"),
                           fxname)
                fyname <- paste0("Expressions/", fname, ".y.R")
                ycode <- readLines(fyname)
                writeLines(c(gsub("expression",
                                  paste0(fname, ".y ",
                                         "<- function",
                                         "(px0, px1, px2, px3, py0, py1, py2, py3, s1, s2, t) ",
                                         "{\n"),
                                  ycode),
                             "}"),
                           fyname)
            }
        }
    }
}
}

xsplineUnitTangentExpr <- function(FUN) {
    tangent <- xsplineTangentExpr(FUN)
    mod <- substitute(sqrt(a^2 + b^2), list(a=tangent$x, b=tangent$y))
    list(x=substitute(a/b, list(a=tangent$x, b=mod)),
         y=substitute(a/b, list(a=tangent$y, b=mod)))         
}

notrun <- function() {
for (i in c("s1pos", "s1neg")) {
    for (j in c("s2pos", "s2neg")) {
        for (k in c("A0", "noA0")) {
            for (m in c("A3", "noA3")) {
                utname <- paste("xsplineUnitTangent", i, j, k, m, sep=".")
                assign(utname,
                       xsplineUnitTangentExpr(get(paste("xsplineFun",
                                                        i, j, k, m,
                                                        sep="."))))
                dput(as.expression(get(utname)$x),
                     file=paste0("Expressions/", utname, ".x.R"))
                dput(as.expression(get(utname)$y),
                     file=paste0("Expressions/", utname, ".y.R"))
            }
        }
    }
}
}

xsplineUnitNormalExpr <- function(ut) {
    dutx <- D(ut$x, "t")
    duty <- D(ut$y, "t")
    mod <- substitute(sqrt(a^2 + b^2), list(a=dutx, b=duty))
    list(x=substitute(a/b, list(a=dutx, b=mod)),
         y=substitute(a/b, list(a=duty, b=mod)))         
}

notrun <- function() {
for (i in c("s1pos", "s1neg")) {
    for (j in c("s2pos", "s2neg")) {
        for (k in c("A0", "noA0")) {
            for (m in c("A3", "noA3")) {
                unname <- paste("xsplineUnitNormal", i, j, k, m, sep=".")
                assign(unname,
                       xsplineUnitNormalExpr(get(paste("xsplineUnitTangent",
                                                       i, j, k, m, sep="."))))
                dput(as.expression(get(unname)$x),
                     file=paste0("Expressions/", unname, ".x.R"))
                dput(as.expression(get(utname)$y),
                     file=paste0("Expressions/", unname, ".y.R"))
            }
        }
    }
}
}

