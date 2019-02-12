
## Make S3 classes available to S4 methods
setOldClass("grob")
setOldClass(c("brushXsplineGrob", "grob"))
setOldClass(c("offsetBezierGrob", "grob"))
setOldClass(c("offsetXsplineGrob", "grob"))
setOldClass(c("vwcurveGrob", "grob"))
setOldClass(c("vwlineGrob", "grob"))
setOldClass("gList")
setClassUnion("gridgrob", c("grob", "gList"))
