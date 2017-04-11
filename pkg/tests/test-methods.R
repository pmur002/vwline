
## Generate a variety of variable-width shapes with different methods

library(vwline)

N <- 1
    
grid.newpage()
pushViewport(viewport(layout=grid.layout(N, 5)))

pushViewport(viewport(layout.pos.row=1, layout.pos.col=1),
             viewport(width=.8, height=.8))
grid.vwcurve(0:1, rep(.5, 2), unit(1, "cm"))
popViewport(2)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=2),
             viewport(width=.8, height=.8))
grid.vwXspline(0:2/2, rep(.5, 3), unit(rep(1, 3), "cm"))
popViewport(2)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=3),
             viewport(width=.8, height=.8))
grid.vwline(0:1, rep(.5, 2), unit(rep(1, 2), "cm"))
popViewport(2)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=4),
             viewport(width=.8, height=.8))
grid.brushXspline(verticalBrush, 0:1, rep(.5, 2), unit(rep(1, 2), "cm"))
popViewport(2)
pushViewport(viewport(layout.pos.row=1, layout.pos.col=5),
             viewport(width=.8, height=.8))
grid.offsetXspline(0:1, rep(.5, 2), unit(rep(1, 2), "cm"))
popViewport(2)

