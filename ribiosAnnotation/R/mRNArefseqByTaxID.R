#' Retrieve mRNA RefSeq records by TaxID
#' @param taxid NCBI Taxonomy ID, supported values include human (9606), mouse (10090), and rat (10116)
#' @return A \code{data.frame} containing RefSeq ids and their annotation
#' @examples
#' \dontrun{
#' ## get all RefSeq records of human genes
#' mRNArefseqByTaxID(9606)
#' }
#' @export mRNArefseqByTaxID
mRNArefseqByTaxID <- function(taxid=10090) {
    taxidQ <-  paste("'", as.character(taxid), "'", sep="")
    comm <- paste("SELECT a.item_id, c.RO_GENE_ID ,c.GENE_SYMBOL, c.DESCRIPTION, i.GENE_TYPE ", 
                  " FROM genome.GTI_GENE_ITEMS a, GTI_GENES c, EG_GENE_INFO i WHERE",
                  " a.item_type_id='4' AND a.ro_gene_id=c.ro_gene_id AND ",
                  "(c.ro_gene_id=i.GENEID AND c.tax_id = i.TAX_ID AND c.TAX_ID=", taxidQ, ")", 
                  sep = "")
    ann <- querydb(comm, "bin", "genome", "genome")
    colnames(ann) <- c("RefSeq", "GeneID", "GeneSymbol", "Description", "GeneType")
    return(ann)
}
