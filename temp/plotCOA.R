library(made4)

ord2expvar <- function(ord) {
  eigens <- ord$ord$ei
  return(eigens/sum(eigens))
}
plotCOA <- function(ord, classvec=NULL, arraycol=NULL, xlim=NULL, ylim=NULL, graph="groups", ...) {
  ord.co <- ord$ord$co
  expvars <- ord2expvar(ord)
  if(is.null(xlim)) {
      xlim <- range(pretty(ord.co[,1]))
  }
  if(is.null(ylim)) {
      ylim <- range(pretty(ord.co[,2]))
  }
  plot(0, 0, xlim=xlim,ylim=ylim, type="n",
       xlab=sprintf("Correspondence Axis 1 (%2.1f%% variance explained)", expvars[1]*100),
       ylab=sprintf("Correspondence Axis 2 (%2.1f%% variance explained)", expvars[2]*100),
       ...)
  abline(h=0, v=0, col="gray70")
  plotarrays(ord.co, classvec=classvec,arraycol=arraycol, grid=FALSE, add.plot=TRUE, cpoint=1.5, labelsize=0.8, graph=graph)
  return(invisible(ord.co))
}
