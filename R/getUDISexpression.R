buildUDISexpURL <- function(studyidexternal, format, query="signals", studydomain="undefined", outdest="browser") {
  UDIS.EXP.CGI <- "http://udisdev.roche.com/udiscgiqa/expressionData_cgi"
  paste(UDIS.EXP.CGI,
        "?query=", query,
        "&studyidexternal=", studyidexternal,
        "&format=", format,
        "&studydomain=", studydomain,
        "&outdest=", outdest, sep="")
}

meta2pd <- function(str) {
  lines <- strsplit(str, "\n")[[1L]]
  sl <- strsplit(lines, "\t")
  ## remove empty lines
  sl <- sl[sapply(sl, length)!=0]
  sl.df <- do.call(cbind, sl)

  df <- data.frame(sl.df[3:nrow(sl.df), , drop=FALSE])
  df.cnames <-  make.unique(make.names(sl.df[1L,, drop=TRUE]))
  colnames(df) <- df.cnames
  metaData <- data.frame(labelDescription=sl.df[2L,,drop=TRUE],
                         rownames=df.cnames)
  pd <- new("AnnotatedDataFrame",
            data=df,
            varMetadata=metaData)
  return(pd)
}

getUDISexpression <- function(studyid="NCS_tissue_rat") {
  SIGNALS.URL <- buildUDISexpURL(studyidexternal=studyid, format="gct", query="signals")
  str <- queryUrl(SIGNALS.URL)
  if(nchar(str)>50 && !grepl("^#err", str) && !grepl("^http", str)) {
    mat <- read_gctstr_matrix(str, keep.desc=TRUE)
    META.URL <- buildUDISexpURL(studyidexternal=studyid, format="matrix", query="meta")
    metastr <- queryUrl(META.URL)
    pd <- meta2pd(metastr)
    sampleNames(pd) <- colnames(mat)
    eset <- new("ExpressionSet", exprs=mat, phenoData=pd)
    rm(str, metastr)
    return(eset)
  } else {
    cat(str)
    cat("\nConnection string:", SIGNALS.URL, "\n")
    return(NULL)
  }
}
