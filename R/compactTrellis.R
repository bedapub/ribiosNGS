#' Return a compact setting for lattice plots, useful for preparing publications
#' @description
#' The function returns a set of lattice options that are useful for compact figures,
#' with less room for padding and therefore more room for the figure. It is often used
#' to prepare for publications.
#'
#' @return
#' A list that can be used in \code{lattice.options}
#'
#' @examples
#' opts <- compactTrellis()

compactTrellis <- function() {
  op <- lattice::col.whitebg()
  op$layout.widths=list(left.padding=0,
    key.ylab.padding=0.5,
    ylab.axis.padding=0.5,
    axis.right=0.5,
    right.padding=0)
  op$layout.heights=list(top.padding=0,
    bottom.padding=0,
    axis.top=0,
    main.key.padding=0.5,
    key.axis.padding=0.5) ## margins are controlled by 'padding' options
  return(op)
}

#' Set compact trellis as default
#' @description The function sets compact trellis options as default
#'
#' @return as \code{lattice.options}. The side-effect is used.
#'
#'@examples
#'\dontrun{
#'setCompactTrellis()
#'}

setCompactTrellis <- function() {
  lattice::lattice.options("default.theme"=compactTrellis())
}

##ibios.yscale.components <- function (lim, packet.number = 0, packet.list = NULL, right = FALSE, ...) {
## comps <- lattice:::calculateAxisComponents(lim, packet.list = packet.list, 
##                                            packet.number = packet.number, ...)
## list(num.limit = comps$num.limit,
##      left = list(ticks = list(at = comps$at, 
##                    tck = 1),
##        labels = list(at = comps$at, labels = comps$labels, 
##          cex = 1, check.overlap = comps$check.overlap)),
##      right = right)
##
##ibios.xscale.components <- function (lim, packet.number = 0, packet.list = NULL, top = FALSE,  ...) 
##
## comps <- lattice:::calculateAxisComponents(lim, packet.list = packet.list, 
##                                            packet.number = packet.number, ...)
## list(num.limit = comps$num.limit,
##      bottom = list(ticks = list(at = comps$at, tck = 1),
##        labels = list(at = comps$at, labels = comps$labels, 
##          check.overlap = comps$check.overlap)),
##      top = top)
##
##ibiosLatticeOptions <- function() {
## op <- lattice.options()
## op$xscale.components <- ribios.xscale.components
## op$yscale.components <- ribios.yscale.components
## op$skip.boundary.labels <- 0
## return(op)
##
##
##ibiosLattice <- function() {
## op <- lattice.options()
## lattice.options(ribiosLatticeOptions())
## return(invisible(op))
##
