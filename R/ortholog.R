safe.as.integer <- function(x) as.integer(as.character(x))

humanOrthologs <- function(geneids) {
  otbl <- ribiosAnnotation:::querydbSelectIn("SELECT RO_GENE_ID1 as HG, RO_GENE_ID2 as NHG FROM GTI_ORTHOLOGS WHERE TAX_ID1='9606' AND ",
                                             inCol="RO_GENE_ID2",
                                             inValues=geneids,
                                             db="bin")
  otbl <- otbl[grepl("^[0-9]*$", otbl[,"HG"]),,drop=FALSE]
  olist <- split(safe.as.integer(otbl[,"HG"]),
                 otbl[,"NHG"])
  ind <- match(geneids, names(olist))
  res <- olist[ind]
  names(res) <- geneids
  return(res)
}

humanUniqOrtholog <- function(geneids) {
  allOrths <- humanOrthologs(geneids)
  sapply(allOrths, function(x) {
    if(length(x)==0) return(NA)
    min(x, na.rm=TRUE)
  })  
}

nonhumanOrthologs <- function(geneids, taxid=NULL) {
  pre.state <- ifelse(is.null(taxid),
                      "SELECT RO_GENE_ID1 as HG, TAX_ID2 as TAXID, RO_GENE_ID2 as NHG FROM GTI_ORTHOLOGS WHERE TAX_ID2!='9606' AND ",
                      paste("SELECT RO_GENE_ID1 as HG, TAX_ID2 as TAXID,",
                            "RO_GENE_ID2 as NHG FROM GTI_ORTHOLOGS WHERE TAX_ID2!='9606' AND ",
                            "TAX_ID2 IN", formatIn(taxid),
                            "AND ",sep=" "))
  
  ntbl <- ribiosAnnotation:::querydbSelectIn(pre.state,
                                             inCol="RO_GENE_ID1",
                                             inValues=geneids,
                                             db="bin")
  ntbl <- ntbl[grepl("^[0-9]*$", ntbl[,"NHG"]),,drop=FALSE]
  nhgs <- safe.as.integer(ntbl[,"NHG"])
  if(is.null(taxid) || length(taxid)>1)
    names(nhgs) <- ntbl[,"TAXID"]
  nlist <- split(nhgs, ntbl[,"HG"])
  ind <- match(geneids, names(nlist))
  res <- nlist[ind]
  names(res) <- geneids
  return(res)
}
nonhumanUniqOrtholog <- function(geneids, taxid) {
  if(missing(taxid) || length(taxid)>1) {
    stop("Exact one TaxID is required to find unique orthologous genes for human genes")
  }
  allOrths <- nonhumanOrthologs(geneids, taxid=taxid)
  sapply(allOrths, function(x) {
    if(length(x)==0) return(NA)
    unname(min(x, na.rm=TRUE))
  })  
}
