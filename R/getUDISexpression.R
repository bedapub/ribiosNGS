##UDIS_DEV_QUERY_CGI <- "http://udisdev.roche.com/udiscgiqa/expressionData_cgi"
UDIS_QUERY_CGI <- "http://udis.roche.com:8080/query/api"

buildUDISexpURL <- function(id,
                            idtype,
                            querytype=c("expr", "meta", "probe"),...) {
  params <- list(...)
  if(length(params)>0) {
    addparams <- paste("&",
                       paste(names(params), sapply(params, "[[", 1L),
                       sep="=", collapse="&"),sep="")
  } else {
    addparams <- ""
  }
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
getUDISexpression <- function(id="GSE20986",idType=c("studyIdExternal", "studyId", "studyTitle", "datasetId"), ...) {
  idType <- match.arg(idType)
  idtype <- tolower(idType)
  turl <- buildUDISexpURL(id=id, idtype=idtype, ...)
  str <- queryUrl(turl)
  if(grepl("^#1.2", str)) { ## valid GCT file
    mat <- read_gctstr_matrix(str, keep.desc=TRUE)
    purl <- buildUDISexpURL(id=id, idtype=idtype, querytype="meta")
    furl <- buildUDISexpURL(id=id, idtype=idtype, querytype="probe", ...)
    metastr <- queryUrl(purl)
    featstr <- queryUrl(furl)
    pd <- meta2pd(metastr)
    fd <- meta2pd(featstr)
    sampleNames(pd) <- colnames(mat)
    featureNames(fd) <- rownames(mat)
    eset <- new("ExpressionSet", exprs=mat, phenoData=pd, featureData=fd)
    rm(str, metastr, featstr)
    return(eset)
  } else {
    cat(str)
    cat("\nConnection string:", turl, "\n")
    return(NULL)
  }
}
