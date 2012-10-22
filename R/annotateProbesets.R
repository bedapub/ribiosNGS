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
annotateHumanOrthologs <- function(geneids, multiOrth=FALSE) {
  geneids <- unique(geneids)
  comm <- paste("SELECT a.RO_GENE_ID2, a.TAX_ID2, 9606 as TaxID, a.RO_GENE_ID1, b.GENE_SYMBOL",
                "FROM genome.GTI_ORTHOLOGS a, genome.GTI_GENES b",
                "WHERE a.RO_GENE_ID1=b.RO_GENE_ID AND a.TAX_ID1 ='9606'")
  ort <- querydbTmpTbl(comm,
                       "a.RO_GENE_ID2",
                       geneids, "bin", ORACLE.BIN.USER, ORACLE.BIN.PWD)
  colnames(ort) <- c("OrigGeneID", "OrigTaxID", "TaxID", "GeneID", "GeneSymbol")
  res <- matchColumn(geneids, ort, "OrigGeneID", multi=multiOrth)
  return(res)
}
annotateHumanOrthologsNoOrigTax <- function(...) {
  res <- annotateHumanOrthologs(...)
  return(res[,-2L])
}

gtiChipAnnotation <- function(chip,ids, orthologue=FALSE, multiOrth=FALSE) {
  if(missing(chip)) stop("'chip' cannot be missing.Using 'gtiChiptypes()' to find supported chip types.")
  
  hasInColVal <- !missing(ids)
  woOrthCn <- c("ProbeID", "GeneID", "GeneSymbol", "GeneName", "Chip", "TaxID")
  wOrthCn <- c(woOrthCn, c("OrigTaxID", "OrigGeneID", "OrigGeneSymbol"))
  
  if(!hasInColVal) {
    state <- paste("SELECT a.PROBESET_ID, e.RO_GENE_ID, e.GENE_SYMBOL, e.DESCRIPTION, a.ARRAY_TYPE, e.TAX_ID ",
                   "FROM genome.chip_probeset_gene a, genome.GTI_GENES e ",
                   "where a.RO_GENE_ID(+) = e.RO_GENE_ID ",
                   "AND ARRAY_TYPE='",chip, "'", sep="")
    ann <- querydb(state, db="bin", user=ORACLE.BIN.USER, password=ORACLE.BIN.PWD)
  } else {
    state <-  paste("SELECT a.PROBESET_ID, e.RO_GENE_ID, e.GENE_Symbol, e.DESCRIPTION, a.ARRAY_TYPE, e.TAX_ID ",
                    "FROM genome.chip_probeset_gene a, genome.GTI_GENES e ",
                    "WHERE a.RO_GENE_ID(+) = e.RO_GENE_ID ",
                    "AND ARRAY_TYPE='",chip, "'",sep="")
    ann <- querydbTmpTbl(state, inCol="a.PROBESET_ID",inValues=ids, db="bin", user=ORACLE.BIN.USER, password=ORACLE.BIN.PWD)
  }

  if(!orthologue) {
    colnames(ann) <- woOrthCn
    res <- ann
  } else {
    colnames(ann) <- c("ProbeID", "OrigGeneID", "OrigGeneSymbol", "GeneName", "Chip", "OrigTaxID")
    ort <- annotateHumanOrthologsNoOrigTax(ann$OrigGeneID, multiOrth=multiOrth)
    if(multiOrth) {
      res <- merge(ann, ort, by="OrigGeneID", all.x=TRUE)
    } else {
      ort.re <- matchColumn(ann$OrigGeneID, ort, "OrigGeneID", multi=FALSE)
      res <- cbind(ann, ort.re[,-1L])
    }
    res <- putColsFirst(res, wOrthCn)
  }
  
  if(hasInColVal) {
    res <- matchColumn(ids, res, "ProbeID", multi=orthologue && multiOrth)
  }
  rownames(res) <- id2rownames(res$ProbeID)
  return(res)
}

annotateChip <- gtiChipAnnotation

## Scenario 2: annotate probesets with unknown, but single chip type
guessChiptype <- function(ids, maxOnly=FALSE, sample=100) {
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
annotateAnyProbeset <- function(ids, orthologue=FALSE, multiOrth=FALSE) {
  comm <- paste("SELECT a.ANY_ID ,a.RO_GENE_ID ,c.GENE_SYMBOL, c.DESCRIPTION, 'NA' AS chip, c.TAX_ID ",
                " FROM genome.GTI_IDMAP a, ",
                " GTI_GENES c",
                " WHERE a.RO_GENE_ID=c.RO_GENE_ID", sep="")
  cnames <- c("ProbeID", "GeneID", "GeneSymbol", "GeneName", "Chip", "TaxID")
  conames <- c(cnames, c("OrigTaxID", "OrigGeneID", "OrigGeneSymbol"))
  ann <- querydbTmpTbl(comm,
                       "a.ANY_ID",
                       ids, "bin", ORACLE.BIN.USER, ORACLE.BIN.PWD)

  
  if(!orthologue) {
    colnames(ann) <- cnames
    res <- ann
  }
  if(orthologue) {
    colnames(ann) <- c("ProbeID", "OrigGeneID", "OrigGeneSymbol", "GeneName", "Chip", "OrigTaxID")
    ort <- annotateHumanOrthologsNoOrigTax(ann$OrigGeneID, multiOrth=multiOrth)
    if(multiOrth) {
      res <- merge(ann, ort, by="OrigGeneID", all.x=TRUE)
    } else {
      ort.re <- matchColumn(ann$OrigGeneID, ort, "OrigGeneID", multi=FALSE)
      res <- cbind(ann, ort.re[,-1L])
    }
    res <- putColsFirst(res, conames)
  }
  res <- matchColumn(ids, res, "ProbeID", multi=orthologue && multiOrth)
  rownames(res) <- id2rownames(res$ProbeID)
  return(res)
}

annotateProbesets <- function(ids, chip, orthologue=FALSE) {
  if(missing(chip) || notValid(chip)) {
    annotateAnyProbeset(ids, orthologue=orthologue)
  } else {
    gtiChipAnnotation(chip, ids=ids, orthologue=orthologue)
  }
}

annotateProbeIDs <- annotateProbesets

annotateGeneIDs <- function(ids, orthologue=FALSE, multiOrth=FALSE) {
  comm <- paste("SELECT c.RO_GENE_ID,c.GENE_SYMBOL, c.DESCRIPTION, c.TAX_ID ",
                " FROM GTI_GENES c", sep="")
  ann <- querydbTmpTbl(comm, 
                       "c.RO_GENE_ID",
                       ids, "bin", ORACLE.BIN.USER, ORACLE.BIN.PWD)
  cnames <- c("GeneID", "GeneSymbol", "GeneName", "TaxID")
  conames <- c("OrigGeneID", "OrigGeneSymbol", "OrigGeneName", "OrigTaxID")

  if(!orthologue) {
    colnames(ann) <- cnames
    cn <- "GeneID"
    res <- ann
  } else {
    colnames(ann) <- conames
    cn <- "OrigGeneID"
    ort <- annotateHumanOrthologsNoOrigTax(ann$OrigGeneID, multiOrth=multiOrth)
    if(multiOrth) {
      res <- merge(ann, ort, by="OrigGeneID", all.x=TRUE)
    } else {
      ort.re <- matchColumn(ann$OrigGeneID, ort, "OrigGeneID", multi=FALSE)
      res <- cbind(ann, ort.re[,-1L])
    }
    res <- putColsFirst(res, c("GeneID", "GeneSymbol", "TaxID",
                               "OrigTaxID", "OrigGeneID", "OrigGeneSymbol", "OrigGeneName"))
  }
  res <- matchColumn(ids, res, cn, multi=orthologue && multiOrth)
  rownames(res) <- id2rownames(res[,cn])
  return(res)
}

annotateGeneSymbols <- function(ids,
                                organism=c("human", "mouse", "rat", "any"),
                                orthologue=FALSE,
                                multiOrth=FALSE) {
  organism <- match.arg(organism)
  oid <- c("any"="", "human"="9606", "mouse"="10090", rat="10116")[organism]
  comm <- paste("SELECT c.RO_GENE_ID,c.GENE_SYMBOL, c.DESCRIPTION, c.TAX_ID ",
                " FROM GTI_GENES c ",
                ifelse(organism=="any", "", paste("WHERE c.TAX_ID='", oid, "' ",sep="")),
                sep="")
  cnames <- c("GeneID", "GeneSymbol", "GeneName", "TaxID")
  ann <- querydbTmpTbl(comm,
                       "c.GENE_SYMBOL",
                       ids, "bin", ORACLE.BIN.USER, ORACLE.BIN.PWD)

  if(!orthologue) {
    colnames(ann) <- cnames
    cn <- "GeneSymbol"
    res <- ann
  } else {
    colnames(ann) <- c("OrigGeneID", "OrigGeneSymbol", "OrigGeneName", "OrigTaxID")
    cn <- "OrigGeneSymbol"
    ort <- annotateHumanOrthologsNoOrigTax(ann$OrigGeneID, multiOrth=multiOrth)
    if(multiOrth) {
      res <- merge(ann, ort, by="OrigGeneID", all.x=TRUE)
    } else {
      ort.re <- matchColumn(ann$OrigGeneID, ort, "OrigGeneID", multi=FALSE)
      res <- cbind(ann, ort.re[,-1L])
    }
    res <- putColsFirst(res, c("GeneID", "GeneSymbol", "TaxID",
                               "OrigTaxID", "OrigGeneID", "OrigGeneSymbol", "OrigGeneName"))
  }
                       
  res <- matchColumn(ids, res, cn, multi=multiOrth)
  rownames(res) <- id2rownames(res[,cn])
  return(res)
}

annotateAnyID <- function(ids, chiptype, orthologue=FALSE) {
  if(identical(tolower(ids), tolower("GeneID"))) {
    annotateGeneIDs(ids=ids, orthologue=orthologue)
  } else if (identical(tolower(ids), tolower("GeneSymbol"))) {
    annotateGeneSymbols(ids=ids, orthologue=orthologue)
  } else {
    annotateProbesets(ids=ids, orthologue=orthologue)
  }
}
