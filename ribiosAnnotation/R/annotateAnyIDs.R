## if only looking after proteins, it is also possible to check the GA_PROTEIN_GENE table in the protein component of the Genome Analysis Pipeline

## note by zhangj83: this file is not yet part of the package. needs to be tested. On 04.05.2015

#' @export annotateAnyIDs
annotateAnyIDs <- function(ids, orthologue = FALSE, multiOrth = FALSE) {
  comm <- paste("SELECT m.ANY_ID, m.ID_TYPE, c.RO_GENE_ID,c.GENE_SYMBOL, c.DESCRIPTION, c.TAX_ID ", 
                " FROM GTI_GENES c JOIN GTI_IDMAP m ON c.RO_GENE_ID=m.RO_GENE_ID ",sep="")
  ann <- querydbTmpTbl(comm, "m.ANY_ID", ids, "bia",
                       ORACLE.BIN.USER, 
                       ORACLE.BIN.PWD)
  cnames <- c("Input", "InputIDType", "GeneID", "GeneSymbol", "GeneName", "TaxID")
  conames <- c("Input", "InputIDType", "OrigGeneID", "OrigGeneSymbol", "OrigGeneName", 
               "OrigTaxID")
  cn <- "Input"
  if (!orthologue) {
    colnames(ann) <- cnames
    res <- ann
  } else {
    colnames(ann) <- conames
    ort <- annotateHumanOrthologsNoOrigTax(ann$OrigGeneID, 
                                           multiOrth = multiOrth)
    if (multiOrth) {
      res <- merge(ann, ort, by = "OrigGeneID", all.x = TRUE)
    }  else {
      ort.re <- matchColumn(ann$OrigGeneID, ort, "OrigGeneID", 
                            multi = FALSE)
      res <- cbind(ann, ort.re[, -1L])
    }
    res <- putColsFirst(res, c("Input", "InputIDType", "GeneID", "GeneSymbol", "TaxID", 
                               "OrigTaxID", "OrigGeneID", "OrigGeneSymbol", "OrigGeneName"))
  }
  res <- matchColumn(ids, res, cn, multi = orthologue && multiOrth)
  rownames(res) <- id2rownames(res[, cn])
  return(res)
}

## testIDs <- c("O43684", "O43670")
## testAnno <- annotateAnyIDs(testIDs)
