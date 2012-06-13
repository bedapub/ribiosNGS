id2rownames <- function(ids) {
  if(identical(anyDuplicated(ids), 0L) & !any(is.na(ids))) {
    return(ids)
  } else {
    return(NULL)
  }
}
notValid <- function(x)  is.null(x) || is.na(x) || tolower(x)=="any" || x==""

annotateIDs <- function(ids, chip="HG-U133_PLUS_2", column="ProbeID") {
  ids <- as.character(ids)
  chipAnn <- gtiChipAnnotation(chip, inCol=column, inValues=ids)
  chipAnnOrd <- matchColumn(ids, chipAnn, column)
  
  rownames(chipAnnOrd) <- id2rownames(ids)
  chipAnnOrd[,column] <- ids
  return(chipAnnOrd)
}

annotateAnyProbesets <- function(ids, orthologue=FALSE) {
  if(orthologue) {
    comm <- paste("SELECT a.ANY_ID ,a.RO_GENE_ID ,b.RO_GENE_ID1, c.GENE_SYMBOL, c.DESCRIPTION, b.TAX_ID2",
                  " FROM genome.GTI_IDMAP a, ",
                  " GTI_ORTHOLOGS b, GTI_GENES c",
                  " WHERE a.RO_GENE_ID=b.RO_GENE_ID2 AND b.TAX_ID1='9606' AND ",
                  " b.RO_GENE_ID1=c.RO_GENE_ID AND", sep="")
    cnames <-  c("ProbeID", "OrigGeneID", "GeneID", "GeneSymbol", "GeneName", "OrigTaxID")
  } else {
    comm <- paste("SELECT a.ANY_ID ,a.RO_GENE_ID ,c.GENE_SYMBOL, c.DESCRIPTION, c.TAX_ID ",
                  " FROM genome.GTI_IDMAP a, ",
                  " GTI_GENES c",
                  " WHERE a.RO_GENE_ID=c.RO_GENE_ID AND ", sep="")
    cnames <- c("ProbeID", "GeneID", "GeneSymbol", "GeneName", "TaxID")
  }
  res <- querydbSelectIn(comm, 
                         "a.ANY_ID",
                         ids, "bin", "genome", "genome")
  colnames(res) <- cnames
    res <- matchColumn(ids, res, "ProbeID")
  res$isSingleGeneID <- TRUE
  res$Chip <- NA
  if(orthologue) res$TaxID <- "9606"
  res <- putColsFirst(res, c("ProbeID", "GeneID", "GeneSymbol", "GeneName", "Chip", "isSingleGeneID"))


  rownames(res) <- id2rownames(ids)
  return(res)
}

annotateProbesets <- function(ids, chip, orthologue=FALSE) {
  if(missing(chip) || notValid(chip)) {
    annotateAnyProbesets(ids, orthologue=orthologue)
  } else {
    annotateIDs(ids=ids, chip=chip, column="ProbeID")
  }
}

annotateProbeIDs <- annotateProbesets

annotateGeneIDs <- function(ids, orthologue=FALSE) {
  if(orthologue) {
    comm <- paste("SELECT c.RO_GENE_ID, b.RO_GENE_ID1, c.GENE_SYMBOL, c.DESCRIPTION, b.TAX_ID2, d.GENE_SYMBOL ",
                  " FROM  GTI_ORTHOLOGS b, GTI_GENES c, GTI_GENES d ",
                  " WHERE b.TAX_ID1='9606' AND b.RO_GENE_ID2=c.RO_GENE_ID AND b.RO_GENE_ID1=d.RO_GENE_ID AND", sep="")
    cnames <-  c("OrigGeneID", "GeneID", "OrigGeneSymbol", "GeneName", "OrigTaxID", "GeneSymbol")
    cn <- "OrigGeneID"
  } else {
    comm <- paste("SELECT c.RO_GENE_ID,c.GENE_SYMBOL, c.DESCRIPTION, c.TAX_ID ",
                  " FROM GTI_GENES c",
                  " WHERE ", sep="")
    cnames <- c("GeneID", "GeneSymbol", "GeneName", "TaxID")
    cn <- "GeneID"
  }
  res <- querydbSelectIn(comm, 
                         "c.RO_GENE_ID",
                         ids, "bin", "genome", "genome")
  colnames(res) <- cnames
  res <- matchColumn(ids, res, cn)
  if(orthologue) res$TaxID <- "9606"
  res <- putColsFirst(res, c("GeneID", "GeneSymbol", "GeneName", "TaxID"))
  rownames(res) <- id2rownames(ids)
  res[,cn] <- ids
  return(res)
}

annotateGeneSymbols <- function(ids, orthologue=FALSE, organism=c("human", "mouse", "rat", "any")) {
  organism <- match.arg(organism)
  oid <- c("any"="", "human"="9606", "mouse"="10090", rat="10116")[organism]
  if(orthologue) {
    comm <- paste("SELECT c.RO_GENE_ID, b.RO_GENE_ID1, c.GENE_SYMBOL, c.DESCRIPTION, b.TAX_ID2, d.GENE_SYMBOL",
                  " FROM  GTI_ORTHOLOGS b, GTI_GENES c, GTI_GENEs d ",
                  " WHERE b.TAX_ID1='9606' AND ",
                  ifelse(organism=="any", "", paste("b.TAX_ID2='", oid, "' AND ",sep="")),
                  "b.RO_GENE_ID2=c.RO_GENE_ID AND b.RO_GENE_ID1=d.RO_GENE_ID AND", sep="")
    cnames <-  c("OrigGeneID", "GeneID", "OrigGeneSymbol", "GeneName", "OrigTaxID", "GeneSymbol")
    cn <- "OrigGeneSymbol"
  } else {
    comm <- paste("SELECT c.RO_GENE_ID,c.GENE_SYMBOL, c.DESCRIPTION, c.TAX_ID ",
                  " FROM GTI_GENES c",
                  " WHERE ",
                  ifelse(organism=="any", "", paste("c.TAX_ID='", oid, "' AND ",sep="")),
                  sep="")
    cnames <- c("GeneID", "GeneSymbol", "GeneName", "TaxID")
    cn <- "GeneSymbol"
  }
  res <- querydbSelectIn(comm, 
                         "c.GENE_SYMBOL",
                         ids, "bin", "genome", "genome")
  colnames(res) <- cnames

  if(orthologue) res$TaxID <- "9606"
  ## NOTE: GeneSymbols are (commonly) NOT unique: therefore multi is required
  res <- matchColumn(ids, res, cn, multi=TRUE)
  res <- putColsFirst(res, c("GeneID", "GeneSymbol", "GeneName", "TaxID"))
  rownames(res) <- NULL
  
  return(res)
}

