setClass("DesignContrast",
         representation=list(design="matrix",
           contrasts="matrix",
           groups="factor"),
         validity=function(object) {
           stopifnot(valid.gd <- length(object@groups)==nrow(object@design))
           stopifnot(valid.dc <- ncol(object@design)==nrow(object@contrasts))
           return(valid.gd & valid.dc)
         })

setMethod("show", "DesignContrast", function(object) {
  cat("DesignContrast object:\n")
  grps <- object@groups
  des <- object@design
  con <- object@contrasts
  cat(sprintf("- %d samples in %d groups\n",
              length(grps), nlevels(grps)))
  cat(sprintf("    Levels: %s\n",
             headtail(levels(grps))))
  cat(sprintf("- Design matrix (%d samples x %d variables)\n",
              nrow(des), ncol(des)))
  cat(sprintf("    Variables: %s\n",
              headtail(colnames(des))))
  cat("  Call 'designMatrix(object)' to get the design matrix.\n")
  cat(sprintf("- Contrast matrix (%d variables x %d contrasts)\n",
              nrow(con), ncol(con)))
  cat(sprintf("    Contrasts: %s\n",
              headtail(colnames(con))))
  cat("  Call 'contrastMatrix(object)' to get the contrast matrix.\n")
})
