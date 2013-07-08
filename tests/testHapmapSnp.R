library(ribiosAnnotation)

snpIds <- c("rs884080", "rs557477", "rs531099", "rs763318")
hapmapSnp(snpIds)
hapmapSnp(snpIds, flanking=TRUE, genes=TRUE)
hapmapSnp(snpIds, flanking=TRUE, genes=FALSE)
hapmapSnp(snpIds, flanking=FALSE, genes=TRUE)
hapmapSnp(snpIds, flanking=FALSE, genes=TRUE)

torture <- FALSE
if(torture) {
  td <- "/DATA/bi/developers/zhangj83/2012-01-bertram/data/secondary_data/pp_genomePthr_FeatureMarkerPairs.txt"
  tbl <- read.table(td, sep="\t",header=TRUE)
  tbl.snps <- as.character(tbl[,2L])
  system.time(test <- hapmapSnp(tbl.snps, genes=TRUE))
  system.time(test2 <- hapmapSnp(tbl.snps, genes=FALSE))
}
