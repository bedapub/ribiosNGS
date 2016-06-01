#' @export
setClass("PcaHubertExt",
         representation=list(biplot.data="matrix"),
         contains="PcaHubert")
