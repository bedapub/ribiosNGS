id2rownames <- function(ids) {
  if(identical(anyDuplicated(ids), 0L) & !any(is.na(ids))) {
    return(ids)
  } else {
    return(NULL)
  }
}
notValid <- function(x)  is.null(x) || is.na(x) || tolower(x)=="any" || x==""

##------------------------------------------------------------##
## Annotate Probesets
## Scenario 1: annotate probesets with known chip type
## Scenario 2: annotate probesets with unknown (single) chip type
## Scenario 3: annotate probesets with mixed chip types
##------------------------------------------------------------##

## Scenario 1: annotate probesets with known chip type
## TODO: gtiChipAnnotation adds support for othologue mapping
gtiChipAnnotation <- function(chip,ids, orthologue=FALSE) {
  if(missing(chip)) stop("'chip' cannot be missing.Using 'gtiChiptypes()' to find supported chip types.")
  
  hasInColVal <- !missing(ids)
  woOrthCn <- c("ProbeID", "GeneID", "GeneSymbol", "GeneName", "Chip", "TaxID")
  wOrthCn <- c(woOrthCn, c("OrigTaxID", "OrigGeneID", "OrigGeneSymbol"))
  if(!hasInColVal) {
    if(!orthologue) {
      state <- paste("SELECT a.PROBESET_ID, e.RO_GENE_ID, e.GENE_SYMBOL, e.DESCRIPTION, a.ARRAY_TYPE, e.TAX_ID ",
                     "FROM genome.chip_probeset_gene a, genome.GTI_GENES e ",
                     "where a.RO_GENE_ID(+) = e.RO_GENE_ID ",
                     "AND ARRAY_TYPE='",chip, "'", sep="")
      ann <- querydb(state, db="bin", user=ORACLE.BIN.USER, password=ORACLE.BIN.PWD)
    } ##else {
      ## state <- paste("SELECT a.PROBESET_ID, e.RO_GENE_ID, e.GENE_SYMBOL, e.DESCRIPTION, a.ARRAY_TYPE, e.TAX_ID, b.TAX_ID2, b.RO_GENE_ID1, ",
      ##               "FROM genome.chip_probeset_gene a, genome.GTI_GENES e ",
      ##               "where a.RO_GENE_ID(+) = e.RO_GENE_ID ",
      ##               "AND ARRAY_TYPE='",chip, "'", sep="")
      ##ann <- querydb(state, db="bin", user=ORACLE.BIN.USER, password=ORACLE.BIN.PWD)
    ##}
  } else {
    if(!is.vector(ids)) stop("'ids' must be a vector")
    state <-  paste("SELECT a.PROBESET_ID, e.RO_GENE_ID, e.GENE_SYMBOL, e.DESCRIPTION, a.ARRAY_TYPE, e.TAX_ID ",
                    "FROM genome.chip_probeset_gene a, genome.GTI_GENES e ",
                    "WHERE a.RO_GENE_ID(+) = e.RO_GENE_ID ",
                    "AND ARRAY_TYPE='",chip, "'",sep="")
    ann <- querydbTmpTbl(state, inCol="a.PROBESET_ID",inValues=ids, db="bin", user=ORACLE.BIN.USER, password=ORACLE.BIN.PWD)
  }
  
  colnames(ann) <- woOrthCn
  rownames(ann) <- NULL
  return(ann)
}

## Scenario 2: annotate probesets with unknown, but single chip type
probesetsChiptype <- function(ids, maxOnly=FALSE, sample=100) {
  ids <- unique(as.character(ids))
  if(!is.null(sample) && !is.na(sample) && is.numeric(sample)) {
    ids <- sample(ids, pmin(as.integer(sample), length(ids)), replace=FALSE)
  }
  state <- paste("SELECT a.PROBESET_ID, a.ARRAY_TYPE ",
                 "FROM genome.chip_probeset_gene a WHERE ")
  ann <- querydbSelectIn(state, inCol="a.PROBESET_ID", inValues=ids,
                         db="bin", user=ORACLE.BIN.USER, password=ORACLE.BIN.PWD)
  res.raw <- sort(table(ann$ARRAY_TYPE), decreasing=TRUE)
  if(maxOnly) res.raw <- res.raw[1]
  res <- names(res.raw)
  attr(res, "count") <- unname(res.raw)
  return(res)
}

## Scenario 3: annotate probesets with mixed chip type
## TODO: add OrigGeneSymbol
annotateAnyProbeset <- function(ids, orthologue=FALSE) {
  if(orthologue) {
    comm <- paste("SELECT a.ANY_ID,a.RO_GENE_ID ,b.RO_GENE_ID1, c.GENE_SYMBOL, c.DESCRIPTION, b.TAX_ID2, b.TAX_ID1",
                  " FROM genome.GTI_IDMAP a, GTI_ORTHOLOGS b, GTI_GENES c",
                  " WHERE b.TAX_ID1='9606' AND a.RO_GENE_ID(+)=b.RO_GENE_ID2 AND ",
                  " b.RO_GENE_ID1=c.RO_GENE_ID ", sep="")

    cnames <-  c("ProbeID", "OrigGeneID", "GeneID", "GeneSymbol", "GeneName", "OrigTaxID", "TaxID")
  } else {
    ## TOO SLOW using TEMP table; Much faster using IN
    ## comm <- paste("SELECT a.PROBESET_ID ,a.RO_GENE_ID ,c.GENE_SYMBOL, c.DESCRIPTION, c.TAX_ID ",
    ##                " FROM genome.CHIP_PROBESET_GENE a, ",
    ##                " GTI_GENES c",
    ##               " WHERE a.RO_GENE_ID=c.RO_GENE_ID", sep="")
    ## commIn <- paste("SELECT a.PROBESET_ID ,a.RO_GENE_ID ,c.GENE_SYMBOL, c.DESCRIPTION, c.TAX_ID ",
    ##              " FROM genome.CHIP_PROBESET_GENE a, ",
    ##              " GTI_GENES c",
    ##              " WHERE a.RO_GENE_ID=c.RO_GENE_ID AND ", sep="")
    comm <- paste("SELECT a.ANY_ID ,a.RO_GENE_ID ,c.GENE_SYMBOL, c.DESCRIPTION, c.TAX_ID ",
                  " FROM genome.GTI_IDMAP a, ",
                  " GTI_GENES c",
                  " WHERE a.RO_GENE_ID=c.RO_GENE_ID", sep="")
    cnames <- c("ProbeID", "GeneID", "GeneSymbol", "GeneName", "TaxID")
  }
  res <- querydbTmpTbl(comm,
                       "a.ANY_ID",
                       ids, "bin", "genome", "genome")
  colnames(res) <- cnames
  res <- matchColumn(ids, res, "ProbeID", multi=FALSE)
  res$Chip <- NA
  if(orthologue) res$TaxID <- "9606"
  res <- putColsFirst(res, c("ProbeID", "GeneID", "GeneSymbol", "GeneName", "Chip", "TaxID"))

  rownames(res) <- id2rownames(ids)
  return(res)
}

annotateProbesets <- function(ids, chip, orthologue=FALSE) {
  if(missing(chip) || notValid(chip)) {
    annotateAnyProbesets(ids, orthologue=orthologue)
  } else {
    ids <- as.character(ids)
    chipAnn <- gtiChipAnnotation(chip, ids=ids)
    chipAnnOrd <- matchColumn(ids, chipAnn, column)
  
    rownames(chipAnnOrd) <- id2rownames(ids)
    chipAnnOrd[,"ProbeID"] <- ids
    return(chipAnnOrd)
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

