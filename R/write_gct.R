write_gct <- function(matrix, file=stdout(), feat.name, feat.desc) {
  if(missing(feat.name)) {
    feat.name <- rownames(matrix)
    if(is.null(feat.name)) {
      warning("'matrix' has NULL as row names, therefore integer indices are used.\n  Consider specifying 'feat.name' instead\n")
      feat.name <- 1:nrow(matrix)
    }
  } else {
    stopifnot(length(feat.name)==nrow(matrix))
  }
  if(!is.null(mdesc <- attr(matrix, "desc"))) {
    feat.desc <- mdesc
  } else if(missing(feat.desc) || is.null(feat.desc)) {
    feat.desc <- ""
  }
  if(identical(file, "")) file <- stdout()
  prefix <- paste("#1.2", "\n", nrow(matrix), "\t", ncol(matrix), 
                  sep = "")
  writeLines(prefix, file)
  df <- data.frame(NAME=feat.name, Description=feat.desc)
  df <- cbind(df, matrix)
  suppressWarnings(write.table(df, file=file, append=TRUE,
                               quote=FALSE, sep="\t",
                               row.names=FALSE, col.names=TRUE, na=""))
}
