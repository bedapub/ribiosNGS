#' @export hapmapSnp
hapmapSnp <- function(ids,
                      genes=FALSE,
                      flanking=FALSE) {
  ## note that the table should be SNP_HAPMAP2
  state.format <- "SELECT %s FROM genome.SNP_HAPMAP2 a "
  state.sel <- c("a.SNP_ID", "a.SEQ", "a.POSITION", "a.ALLELE1", "a.ALLELE2")
  cnames <- c("SNP_ID","Chromosome",  "Position", "Allele1", "Allele2")
  if(genes) {
    state.format <- paste(state.format,
                          ", genome.snp_hapmap_gene b WHERE a.SNP_ID(+)=b.SNP_ID AND ")
    state.sel <- c(state.sel,
                   "b.RO_GENE_ID", "b.DISTANCE", "b.GENE_START", "b.GENE_STOP")
    cnames <- c(cnames, c("ClosestGeneID", "DistanceToGene", "GeneStart", "GeneStop"))
  } else {
    state.format <- paste(state.format, "WHERE ")
  }

  if(flanking) {
    state.sel <- c(state.sel, "FLANKING")
    cnames <- c(cnames, "FlankingSeq")
  }
  state <- sprintf(state.format,
                   paste(state.sel, collapse=","))
  ann <- querydbSelectIn(state,
                         "a.SNP_ID", ids,
                         db="bin", user=ORACLE.BIN.USER, password=ORACLE.BIN.PWD)
  colnames(ann) <- cnames
  ann$Chromosome <- gsub("^CHR", "", ann$Chromosome)
  
  res <- matchColumn(ids, ann, "SNP_ID", multi=genes)
  rownames(res) <- NULL
  return(res)
}

