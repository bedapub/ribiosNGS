## customed Cytoscape/yEd-friendly export function for GML
fixNodeLabel <- function(name) {
  res <- vector("character", length=length(name))
  invalid <- grepl("^-*$", name)
  res[invalid] <- ""
  res[!invalid] <- name[!invalid]
  return(res)
}

#' @export
exportGML <- function(igraph, filename) {
  file <- file(filename, "w")
  cat("Creator \"ribiosNetwork\"\n", file=file)
  cat("Version 1.0\n", file=file)
  cat("graph\n[\n", file=file)
  cat("  directed", as.integer(is.directed(igraph)), "\n", file=file)
  for (i in seq_len(vcount(igraph))) {
    cat("  node\n  [\n", file=file)
    cat("    id", i-1, "\n", file=file)
    cat("    name \"", V(igraph)$name[i], "\"\n", file=file, sep="")
    cat("    label \"", fixNodeLabel(V(igraph)$label[i]), "\"\n", file=file, sep="")
    cat("    graphics\n    [\n", file=file)
    cat("      w 45 \n", file=file, sep="")
    cat("      h 45 \n", file=file, sep="")
    cat("      type \"", ifelse(V(igraph)$isInput[i]==1, "ellipse", "diamond"), "\"\n", file=file, sep="")
    cat("    ]\n", file=file)
    cat("  ]\n", file=file)
  }
  el <- get.edgelist(igraph, names=FALSE)
  min.ind <- min(el)
  for (i in seq_len(nrow(el))) {
    elabel <- E(igraph)$label[i]
    cat("  edge\n  [\n", file=file)
    cat("    source", el[i,1]-min.ind, "\n", file=file)
    cat("    target", el[i,2]-min.ind, "\n", file=file)
    cat("    label \"",  elabel, "\"\n", file=file, sep="")
    cat("    graphics\n    [\n", file=file)
    cat("      source_arrow 0\n", file=file, sep="")
    target.arrow <- 12
    if(length(elabel)==1) {
      if(grepl("positive", elabel, ignore.case=TRUE)) {
        target.arrow <- 3
      } else if (grepl("negative", elabel, ignore.case=TRUE)) {
        target.arrow <- 15
      }
    }
    cat("      target_arrow ", target.arrow, "\n", file=file, sep="")
    cat("    ]\n", file=file)
    cat("  ]\n", file=file)
  }
  cat("]\n", file=file)
  close(file)
}
