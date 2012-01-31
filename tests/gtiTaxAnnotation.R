library(ribiosAnnotation)

hsTax <- "9606"
hsAnno <- gtiTaxAnnotation(hsTax)
stopifnot(all(c("GeneID", "GeneSymbol", "Synonyms", "xrefs", "Chromosome", "MapLocation", "GeneType") %in% colnames(hsAnno)))
stopifnot(any(!is.na(hsAnno$GeneID)))
