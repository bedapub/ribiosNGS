vec2mat <- function(named.list,
                    sort.by=c("alphabetic", "mean", "min", "max"),
                    decreasing=FALSE) {
  sort.by <- match.arg(sort.by)
  univ <- unique(unlist(lapply(named.list, names)))
  if(is.null(univ)) stop("error in vec2mat. Please contact the developer")
  ouniv <- sort(univ)
  ofdrs <- lapply(named.list, function(x) x[match(ouniv, names(x))])
  res <- do.call(cbind, ofdrs)
  if(sort.by=="alphabetic") {
    ord <- 1:nrow(res)
  } else if (sort.by=="mean") {
    ord <- order(rowMeans(res,na.rm=TRUE), decreasing=decreasing)
  } else if (sort.by=="max") {
    ord <- order(apply(res,1L, max, na.rm=TRUE), decreasing=decreasing)
  } else if (sort.by=="min") {
    ord <- order(apply(res,1L, min, na.rm=TRUE), decreasing=decreasing)
  }
  res <- res[ord,,drop=FALSE]
  return(res)
}

parseDTG <- function(xmlNode) {
  gsAttrs <- xmlAttrs(xmlNode)

  ## excluding fields
  ## rankedList <- sapply(gsAttrs, "[[", "RANKED_LIST")
  ## template <- sapply(gsAttrs, "[[", "TEMPLATE")
  ## chip <- sapply(gsAttrs, "[[", "CHIP")
  ## rndes <- sapply(gsAttrs, "[[", "RND_ES") ## permutation ES scores -- not really interesting
  ## rankAtES <- as.numeric(sapply(gsAttrs, "[[", "RANK_AT_ES"))
  
  geneset.raw <- gsAttrs[["GENESET"]]
  geneset <- gsub("^gene_sets.gmt#", "", geneset.raw)
  es <- as.numeric(gsAttrs[["ES"]])
  nes <- as.numeric(gsAttrs[["NES"]])
  np <- as.numeric(gsAttrs[["NP"]])
  fdr <- as.numeric(gsAttrs[["FDR"]])
  fwer <- as.numeric(gsAttrs[["FWER"]])
  geneIndices <- as.integer(strsplit(gsAttrs[["HIT_INDICES"]], " ")[[1]])+1L ## +1 because the original indices are zero based, instead of one based.
  esprofile <- as.numeric(strsplit(gsAttrs[["ES_PROFILE"]], " ")[[1]])
  coreEnrichThr <- as.numeric(gsAttrs[["RANK_SCORE_AT_ES"]])
  obj <- new("gseaResItem",
             geneset=geneset,
             es=es,nes=nes, np=np, fdr=fdr, fwer=fwer,
             geneIndices=geneIndices, esProfile=esprofile, coreEnrichThr=coreEnrichThr)
  return(obj)
}
## parse results.edb, .gmt file, and the rank file to get leading genes
parseGSEAedb <- function(edbfile) {
  doc <- xmlTreeParse(edbfile, getDTD=FALSE)
  root <- xmlRoot(doc)
  children <- xmlChildren(root)
  childrenNames <- sapply(children, xmlName)
  isDTG <- childrenNames=="DTG"
  gsItems <- children[isDTG]
  gsList <- sapply(gsItems, parseDTG)
  names(gsList) <- sapply(gsList, gsName)
  return(gsList)
}
parseGSEAres <- function(edbFile, rnkFile) {
  assertFile(edbFile); assertFile(rnkFile)

  gsList <- parseGSEAedb(edbFile)
  rnk <- read.table(rnkFile, header=FALSE, sep="\t", quote="\"",stringsAsFactors=FALSE)
  haltifnot(ncol(rnk)==2,
            msg="rnkFile must contain two columns: the first with gene identifiers and the second with ranking values")
  agsList <- lapply(gsList, function(x) {
    indices <- gsGeneIndices(x)
    annoGseaResItem(x,
                    genes=rnk[indices,1L],
                    genevalues=rnk[indices,2L])
  })
  return(annoGseaRes(agsList))
}
parseGSEAdir <- function(dir) {
  rnkfile <- dir(file.path(dir, "edb"), pattern="*.rnk", full.name=TRUE)
  edbfile <- dir(file.path(dir, "edb"), pattern="*.edb", full.name=TRUE)
  parseGSEAres(edbFile=edbfile, rnkFile=rnkfile)
}
