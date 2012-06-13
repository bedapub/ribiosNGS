gtiTaxAnnotation <- function(taxid) {
  if(missing(taxid))
    stop("'taxid' cannot be missing.")
  state <- paste("SELECT a.*, b.SEQ, b.CLEFT, b.CRIGHT, b.REVCOMP FROM (SELECT TO_CHAR(g.GENEID) as RO_GENE_ID, g.OFFICIAL_SYMBOL, g.OFFICIAL_NAME, g.SYNONYMS, g.DBXREFS, ",
                 "g.CHROMOSOME, g.MAP_LOCATION, g.GENE_TYPE, ",
                 "g.TAX_ID ",
                 "FROM bi.EG_GENE_INFO g ",
                 "where g.TAX_ID = '",taxid, "') a, genome.gti_gene_map_a@genome b ",
                 "WHERE a.RO_GENE_ID=b.RO_GENE_ID(+)", sep="")
  ann <- querydb(state, db="bia", user=ORACLE.BIA.USER, password=ORACLE.BIA.PWD)
  colnames(ann) <- c("GeneID", "GeneSymbol", "GeneName",
                     "Synonyms", "Xrefs",
                     "Chromosome", "MapLocation",
                     "GeneType","TaxID",
                     "MappedChr", "CoordLeft", "CoordRight", "RevComp")
  ##post processing
  ann$RevComp <- ifelse(ann$RevComp==1L, TRUE, FALSE)
  ann$MappedChr <- gsub("^CHR", "", as.character(ann$MappedChr))
  rownames(ann) <- NULL
  return(ann)
}
