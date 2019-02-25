#' Get the name of the column which store unadjusted P-values from topTables
#' @param colnames A character string vector of column names
#' @return The column name of the unadjusted p-values, \code{NA} if not found.
#' 
#' @examples 
#' getPvalCol(c("Feature", "logFC", "PValue", "FDR"))
#' getPvalCol(c("Feature", "logFC", "P.Value", "FDR"))
#' getPvalCol(c("Feature", "logFC", "p.Value", "adjPvalue"))
#' getPvalCol(c("Feature", "logFC", "pval", "adjPvalue"))
#' @export
getPvalCol <- function(colnames) {
  lcns <- tolower(gsub("[[:punct:]]", "", colnames))
  pvalInd <- grepl("^p", lcns) & !grepl("adj", lcns)
  if(sum(pvalInd)==0)
    return(NA)
  return(colnames[pvalInd])
}

#' Get the name of the column which store false-discovery rates (adjusted P-values) from topTables
#' @param colnames A character string vector of column names
#' @return The column name of the FDRs, \code{NA} if not found.
#' 
#' @examples 
#' getFDRCol(c("Feature", "logFC", "PValue", "FDR"))
#' getFDRCol(c("Feature", "logFC", "P.Value", "FDR"))
#' getFDRCol(c("Feature", "logFC", "p.Value", "adjPvalue"))
#' getFDRCol(c("Feature", "logFC", "PValue", "adj.PValue"))
#' @export
getFDRCol <- function(colnames) {
  lcns <- tolower(gsub("[[:punct:]]", "", colnames))
  fdrInd <- grepl("adj", lcns) | grepl("fdr", lcns)
  if(sum(fdrInd)==0)
    return(NA)
  return(colnames[fdrInd])
}
