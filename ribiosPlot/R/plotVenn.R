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

#' Plot Venn object of the Vennerable package
#' @description
#' The function plots Venn object of the Vennerable package in a way that better suits my eyes
#'
#' @param venn Venn object from the Vennerable package
#' @param main Figure title
#' @param show default options
#' @param ... other parameters passed to plotVenn
#' @return Side effect is used - a plot is generated
#'
#' @note The function is applicable to Vennerable package version 3.0.
#' 
#' @examples
#' if(require("Vennerable")) {
#'  myVenn <- list(A=LETTERS[1:24], B=LETTERS[3:8], C=LETTERS[5:9])
#'  plotVenn(Venn(myVenn), main="Letters")
#' }

plotVenn <- function(venn,
                     main="",
                     show=list(FaceText="weight", Universe=FALSE),...) {
    replaceFunc("makevp.eqsc", makevp.eqsc.mod, package="Vennerable")
    Vennerable:::plotVenn(venn, show=show,...)
    grid.text(main, y=unit(0.95, "npc"), gp=gpar(fontsize=16, font=2))
}
