#' @importFrom methods new
#' @importFrom utils read.csv
#' @importFrom ribiosAuth bioinfoCookie
#' @importFrom ribiosIO read_gctstr_matrix
#' @importFrom Biobase sampleNames<- featureNames<- pData exprs
#' @importFrom RCurl postForm curlOptions


##UDIS_DEV_QUERY_CGI <- "http://udisdev.roche.com/udiscgiqa/expressionData_cgi"
UDIS_QUERY_CGI <- "http://udis.roche.com:8080/query/api"

list2cgipar <- function(list) {
  if(length(list)) {
    return(paste("&",
                 paste(names(list), sapply(list, paste, collapse="|"),  ## UDIS uses | to separate multiple query items
                       sep="=", collapse="&"),sep=""))
  } else {
    return("")
  }
}

buildUDISexpURL <- function(id,
                            idtype,
                            querytype=c("expr", "meta", "probe"),addparams) {
  querytype <- match.arg(querytype)
  paste(UDIS_QUERY_CGI,
        "?entitytype=analysisgroup",
        "&querytype=",querytype,
        "&", idtype, "=", id,
        "&format=matrix",addparams, sep="")
}


meta2pd <- function(str) {
  ## note how to use textConnection to read csv from string
  con <- textConnection(str)
  df <- read.csv(con, sep="\t", header=TRUE)
  close(con)
  metaData <- data.frame(labelDescription=colnames(df),
                         row.names=colnames(df))
  pd <- new("AnnotatedDataFrame",
            data=df,
            varMetadata=metaData)
  return(pd)
}

## As of Feb 2013, NCS_tissue_rat does not work. Use GSE20986 as example
## TODO (David): Add verbose mode
## TODO (David): Design another function to more complicated queries

#' @export getUDISexpression
getUDISexpression <- function(id="GSE20986",idType=c("studyIdExternal", "studyId", "studyTitle", "datasetId"), ...) {
  idType <- match.arg(idType)
  idtype <- tolower(idType)

  addparams <- list2cgipar(list(...))
  
  turl <- buildUDISexpURL(id=id, idtype=idtype, querytype="expr", addparams)
  str <- queryUrl(turl)
  if(grepl("^#1.2", str)) { ## valid GCT file
    mat <- read_gctstr_matrix(str, keep.desc=TRUE)
    purl <- buildUDISexpURL(id=id, idtype=idtype, querytype="meta", addparams)
    furl <- buildUDISexpURL(id=id, idtype=idtype, querytype="probe", addparams)
    metastr <- queryUrl(purl)
    featstr <- queryUrl(furl)
    pd <- meta2pd(metastr)
    ## pheno data row names do not necessarily match the exprs matrix columns. So DONOT USE THESE CODES.
    ## if(nrow(pd)!=ncol(mat) || !identical(sampleNames(pd), colnames(mat))) {
    ##  pd.ind <- match(colnames(mat), sampleNames(pd))
    ##  pd <- pd[pd.ind,]
    ## }
    sampleNames(pd) <- colnames(mat)
    
    fd <- meta2pd(featstr)
    ## the following codes are specific to the current setting of UDIS, where the feature annotation is not garanteed to be in the same order as the expression matrix
    fdFeatNames <- pData(fd)[,1L]
    if(nrow(fd)!=nrow(mat) || !identical(rownames(mat), fdFeatNames)) {
      fd.ind <- match(rownames(mat), fdFeatNames)
      fd <- fd[fd.ind,]
    }
    featureNames(fd) <- rownames(mat)
    
    eset <- new("ExpressionSet", exprs=mat, phenoData=pd, featureData=fd)
    rm(str, metastr, featstr)
    return(eset)
  } else {
    warning("Error in getting data from UDIS:\n")
    cat(str)
    cat("\nConnection string:", turl, "\n")
    return(NULL)
  }
}
