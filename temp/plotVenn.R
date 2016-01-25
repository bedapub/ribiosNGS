plotVenn <- function(venn, main="", show=list(FaceText="weight", Universe=FALSE)
,...) {
  plot(venn, show=show,...)
  grid.text(main, y=unit(0.95, "npc"), gp=gpar(fontsize=16, font=2))
}

makevp.eqsc.mod <- function (xrange, yrange) {
    pushViewport(plotViewport(name = "Vennmar", c(1.5, 1, 3, 1)))
    pushViewport(viewport(name = "Vennlay", layout = grid.layout(1, 
        1, widths = diff(xrange), heights = diff(yrange), respect = TRUE)))
    pushViewport(viewport(name = "Vennvp", layout.pos.row = 1, 
        layout.pos.col = 1, xscale = xrange, yscale = yrange))
}

replaceFunc <- function(funcName,
                        newFunc, package) {
    env.name <- paste("package", package, sep=":")
    env <- as.environment(env.name)
    unlockBinding(funcName, env)
    assignInNamespace(funcName, newFunc, ns=package, envir=env)
    assign(funcName, newFunc, envir=env)
    lockBinding(funcName, env)
}

replaceFunc("makevp.eqsc", makevp.eqsc.mod, package="Vennerable")
