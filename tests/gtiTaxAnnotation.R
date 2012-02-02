library(ribiosAnnotation)

hsTax <- "9606"
system.time(hsAnno <- gtiTaxAnnotation(hsTax))
stopifnot(all(c("GeneID", "GeneSymbol", "Synonyms", "Xrefs", "Chromosome", "MapLocation", "GeneType", "MappedChr", "CoordLeft", "CoordRight", "RevComp") %in% colnames(hsAnno)))
stopifnot(any(!is.na(hsAnno$GeneID)))
