## convert grp file to gmt file
grp2gmt <- function(txt, chiptype, name) {
  if(missing(chiptype) || is.na(chiptype) || is.null(chiptype)) {
    symbols <- txt
  } else {
    symbols <- annotate(object=txt,
                        target=chiptype,
                        check.target=TRUE)$GeneSymbol
  }
  symbols <- symbols[!is.na(symbols)]
  
  if(missing(name))
    name <- "grp"
  
  gmt <- paste(name,
               "grp2gmt",
               paste(symbols, collapse="\t"),
               sep="\t")
  return(gmt)
}

grpFiles2gmt <- function(..., chiptype, n=-1L) {
  files <- unlist(list(...), use.names=FALSE)
  if(missing(chiptype))
    chiptype <- NA

  res <- lapply(files, function(f) {
    grp2gmt(readLines(f, n=n),
            chiptype=chiptype,
            name=gsub("\\..*", "", basename(f)))
  })
  return(unlist(res, use.names=FALSE))
}

## print strings in GMT format
setGeneric("formatGmt",
           function(title, comment, genes)
           standardGeneric("formatGmt"))
setMethod("formatGmt",
          c("character", "character", "character"),
          function(title, comment,genes) {
            if(length(title)!=1L)
              stop("'title' must be a character string")
            if(length(comment)!=1L)
              stop("'comment' must be a character string")
            genes <- unique(genes)
            genes.collapse <- paste(genes, collapse="\t")
            paste(title, comment, genes.collapse,sep="\t")
          })
setMethod("formatGmt",
          c("character", "missing", "character"),
          function(title, genes) {
            formatGmt(title, "", genes)
          })
setMethod("formatGmt",
          c("character", "character", "list"),
          function(title, comment,genes) {
            if(!identical(length(title), length(genes))) {
              stop("'genes' must be a list of the same length as the character vector 'titles'")
            }
            if(length(comment)==1)
              comment <- rep(comment, length(title))
            stopifnot(identical(length(title), length(comment)))
            sapply(1:length(genes),
                   function(x) formatGmt(title[x], comment[x], genes[[x]])
                   )
          })
setMethod("formatGmt",
          c("character", "missing", "list"),
          function(title, genes) {
            formatGmt(title, "", genes)
          })
