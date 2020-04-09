#' Return the top table in a unified format
#' @param edgeResult An \code{EdgeResult} object
#' @param contrast Character, contrast name of interest. If \code{NULL}, 
#'   all tables are returned in a rbind-form.
#' @return A \code{data.frame}
#' @export
dgeTable <- function(edgeResult, contrast=NULL) {
    tbls <- edgeResult@dgeTables
    if(is.logical(contrast) || is.numeric(contrast)) {
        contrast <- contrastNames(edgeResult)[contrast]
    }
    if(!is.null(contrast)) {
        if(length(contrast)==0) {
            stop("No contrast selected")
        } else if (!all(contrast %in% contrastNames(edgeResult))) {
            stop("Following contrasts are not found:",
                 setdiff(contrast, contrastNames(edgeResult)))
        }
    }
    
    if(!is.null(contrast) && length(contrast)==1) {
        res <- tbls[[contrast]]
        res$Contrast <- contrast
    } else {
        if(is.null(contrast)) {
            res <- do.call(rbind, tbls)
            res$Contrast <- rep(contrastNames(edgeResult), sapply(tbls, nrow))
        } else {
            subtbls <- tbls[contrast]
            res <- do.call(rbind, subtbls)
            res$Contrast <- rep(contrast, sapply(subtbls, nrow))
        }
    }
    res <- putColsFirst(res, "Contrast")
    rownames(res) <- NULL
    return(res)
}

#' Return the top tables of specified contrast(s) in a list
#' @param edgeResult An \code{EdgeResult} object
#' @param contrast Character, contrast name(s) of interest. If \code{NULL}, 
#'   all tables are returned in a rbind-form.
#' @return A list of \code{data.frame}s
#' @export
dgeTableList <- function(edgeResult, contrast=NULL) {
  tbls <- edgeResult@dgeTables
  if(is.null(contrast)) {
    res <- tbls
  } else {
    res <- tbls[contrast]
  }
  return(res)
}

#' Return a list of differential gene expression tables 
#' 
#' @param edgeResult An \code{EdgeResult} object
#' 
#' @return A list of \code{data.frame}s, each containing the DGEtable for one contrast.
#' 
#' @seealso \code{dgeTable} which returns one \code{data.frame} for one or more given contrasts.
#' @export
dgeTables <- function(edgeResult) {
  contrs <- contrastNames(edgeResult)
  res <- lapply(contrs, function(ctr) dgeTable(edgeResult, contrast=ctr))
  names(res) <- contrs
  return(res)
}

