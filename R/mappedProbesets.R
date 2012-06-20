mappedProbesets <- function(geneids,
                            chip="HG-U133_PLUS_2",
                            unlist=FALSE) {
  ids <- as.character(geneids)
  state <-  paste("SELECT a.PROBESET_ID, e.RO_GENE_ID, e.GENE_Symbol, e.DESCRIPTION, a.ARRAY_TYPE, e.TAX_ID ",
                  "FROM genome.chip_probeset_gene a, genome.GTI_GENES e ",
                  "WHERE a.RO_GENE_ID(+) = e.RO_GENE_ID ",
                  "AND ARRAY_TYPE='",chip, "'",sep="")
  ann <- querydbTmpTbl(state, inCol="e.RO_GENE_ID",inValues=ids, db="bin", user=ORACLE.BIN.USER, password=ORACLE.BIN.PWD)
  colnames(ann) <- c("ProbeID", "GeneID", "GeneSymbol", "GeneName", "Chip", "TaxID")
  mapId <- with(ann, split(ProbeID, GeneID))
  if(unlist)
    mapId <- unlist(mapId, use.names=FALSE)
  return(mapId)
}
