ISA2gmt <- function(isaRes,
                    eset,
                    sample.class,
                    feature.class,
                    modules,
                    minClassRep=2) {
  if(missing(modules))
    modules <- seq_len(ncol(isaRes@genes))
  
  res <- vector("list", length(modules))
  all.features <- getFeatureNames(isaRes, modules)
  all.featureClasses <- getFeatureAnnotations(isaRes, eset, feature.class, modules)
  all.samples <- getSampleNames(isaRes, modules)
  all.sampleClasses <- lapply(getSampleAnnotations(isaRes, eset, sample.class, modules), as.character)
  all.featureScores <- getFeatureScores(isaRes, modules)

  for(i in seq(along=modules)) {
    module <- modules[i]
    
    samples <- all.samples[[i]]
    sampleClasses <- all.sampleClasses[[i]]
    
    hasRepClass <- table(sampleClasses) >= 2
    if(!any(hasRepClass)) {
      res[[i]] <- NULL
    } else {
      features <- all.features[[i]]
      featureClasses <- as.character(all.featureClasses[[i]])
      
      module.prefix <- sprintf("ISAModule%d_thrGene%s_thrCond%s_%%s_%%s\tISA Bicluster Transcriptional Modules\t%%s", 
                               module, 
                               seedData(isaRes)$thr.row[module],
                               seedData(isaRes)$thr.col[module])
      geneScores <- all.featureScores[[i]]
      isPosScore <- geneScores > 0
      isNegScore <- geneScores < 0

      repClasses <- names(hasRepClass)[hasRepClass]
      
      gmt.pos <- sprintf(module.prefix,
                         repClasses , "UP", 
                         paste(featureClasses[isPosScore],collapse="\t"))
      gmt.neg <- sprintf(module.prefix,
                         repClasses, "DOWN", 
                         paste(featureClasses[isNegScore],collapse="\t"))
      
      if(all(isPosScore)) {
        gmt <- gmt.pos
      } else if (all(isNegScore)) {
        gmt <- gmt.neg
      } else {
        gmt <- c(gmt.pos, gmt.neg)
      }
      res[[i]] <- gmt
    }
  }
  
  res <- unlist(res)
  return(res)
}
