fixNodeLabel <- function(name) {
  res <- vector("character", length=length(name))
  invalid <- grepl("^-*$", name)
  res[invalid] <- ""
  res[!invalid] <- name[!invalid]
  return(res)
}

#' Export igraph object to GML, friendly to Cytoscape and yEd
#' 
#' @param igraph An \code{igraph} object
#' @param filename Filename
#'
#'   \code{exportGML} exports an \code{igraph} object to GML files
#' complying with specifications defined by Cytoscape and yEd. Compared
#'  to the native \code{write.graph} function provided by the
#'  \code{igraph} package, GML files exported with \code{exportGML} can be
#'  directly read and properly visualized by Cytoscape and yEd.
#'
#'   Currently the function uses supports following attributes:
#'  Node name: \code{V(igraph)$name}
#'  Node label: \code{V(igraph)$label}
#'  Node isInput: \code{V(igraph)$isInput}, controlling node shapes
#'  Edge label: \code{V(igraph)$label}, determining edge target arrow
#'    
#'  So far the function is mainly used by the \code{ronet.Rscript} script
#'  in the package. Users are invited to adapt the function for other purposes.
#'
#' @author Jitao David Zhang, \email{jitao_david.zhang@roche.com}
#' @importFrom igraph is.directed vcount V E get.edgelist
#' @seealso \code{\link[igraph]{write.graph}}
#' @examples
#'  g <- barabasi.game(100, directed=FALSE)
#'  V(g)$label <- c(paste("node", 1:99, sep=""),"--")
#'  V(g)$name <- 1:100 
#'  V(g)$isInput <- rbinom(100,1, 0.5)
#'  E(g)$label <- "Expression"
#'  gPosE <- as.logical(rbinom(ecount(g), 1, 0.25))
#'  gNegE <- as.logical(rbinom(ecount(g), 1, 0.25))
#'  E(g)$label[gPosE] <- "Expressoion_Positive"
#'  E(g)$label[gNegE] <- "Expressoion_Negative"
#'  gFile <- tempfile()
#'  exportGML(g, gFile)
#'
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
