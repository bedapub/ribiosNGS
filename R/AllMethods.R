setGeneric("gsName", function(object, ...) standardGeneric("gsName"))
setGeneric("gsDesc", function(object, ...) standardGeneric("gsDesc"))
setGeneric("gsGenes", function(object,...) standardGeneric("gsGenes"))
setGeneric("gsGeneValues", function(object) standardGeneric("gsGeneValues"))
setGeneric("gsGenes<-", function(object,value) standardGeneric("gsGenes<-"))
setGeneric("gsGeneValues<-", function(object,value) standardGeneric("gsGeneValues<-"))
setGeneric("isGseaCoreEnrich", function(object) standardGeneric("isGseaCoreEnrich"))
setGeneric("gseaES", function(object) standardGeneric("gseaES"))
setGeneric("gseaNES", function(object) standardGeneric("gseaNES"))
setGeneric("gseaNP", function(object) standardGeneric("gseaNP"))
setGeneric("gseaFDR", function(object) standardGeneric("gseaFDR"))
setGeneric("gseaFWER", function(object) standardGeneric("gseaFWER"))

setGeneric("gsGeneIndices", function(object) standardGeneric("gsGeneIndices"))
setGeneric("gseaESprofile", function(object) standardGeneric("gseaESprofile"))
setGeneric("gseaCoreEnrichThr", function(object) standardGeneric("gseaCoreEnrichThr"))
setGeneric("gseaCoreEnrichGenes", function(object) standardGeneric("gseaCoreEnrichGenes"))
setGeneric("annoGseaResItem", function(object, ...) standardGeneric("annoGseaResItem"))
##setGeneric("gseaRes", function(object) standardGeneric("gseaRes"))
setGeneric("annoGseaRes", function(object) standardGeneric("annoGseaRes"))

setMethod("gsName", "gseaResItem", function(object) return(object@geneset))
setMethod("gsName", "annoGseaRes", function(object) sapply(object, gsName))
setMethod("gsName", "GeneSets", function(object, i) {
  res <- sapply(object, function(x) x$name)
  if(!missing(i) && !is.null(i)) res <- res[i]
  return(res)
})

setMethod("gseaES", "gseaResItem", function(object) return(object@es))
setMethod("gseaES", "annoGseaRes", function(object) {
  res <- sapply(object, gseaES)
  names(res) <- gsName(object)
  return(res)
})
setMethod("gseaES", "annoGseaResList", function(object) {
  es <- lapply(object, gseaES)
  res <- vec2mat(es, sort.by="mean", decreasing=FALSE)
  return(res)
})

setMethod("gseaNES", "gseaResItem", function(object) return(object@nes))
setMethod("gseaNES", "annoGseaRes", function(object) {
  res <- sapply(object, gseaNES)
  names(res) <- gsName(object)
  return(res)
})
setMethod("gseaNES", "annoGseaResList", function(object) {
  nes <- lapply(object, gseaNES)
  res <- vec2mat(nes, sort.by="mean", decreasing=FALSE)
  return(res)
})

setMethod("gseaNP", "gseaResItem", function(object) return(object@np))
setMethod("gseaNP", "annoGseaRes", function(object) {
  res <- sapply(object, gseaNP)
  names(res) <- gsName(object)
  return(res)
})
setMethod("gseaNP", "annoGseaResList", function(object) {
  nps <- lapply(object, gseaNP)
  res <- vec2mat(nps, sort.by="mean", decreasing=FALSE)
  return(res)
})

setMethod("gseaFDR", "gseaResItem", function(object) return(object@fdr))
setMethod("gseaFDR", "annoGseaRes", function(object) {
  res <- sapply(object, gseaFDR)
  names(res) <- gsName(object)
  return(res)
})
setMethod("gseaFDR", "annoGseaResList", function(object) {
  fdrs <- lapply(object, gseaFDR)
  res <- vec2mat(fdrs, sort.by="mean", decreasing=FALSE)
  return(res)
})

setMethod("gseaFWER", "gseaResItem", function(object) {return(object@fwer)})
setMethod("gseaFWER", "annoGseaRes", function(object) {
  res <- sapply(object, gseaFWER)
  names(res) <- gsName(object)
  return(res)
})
setMethod("gseaFWER", "annoGseaResList", function(object) {
  fwers <- lapply(object, gseaFWER)
  res <- vec2mat(fwers, sort.by="mean", decreasing=FALSE)
  return(res)
})
          
setMethod("gsGeneIndices", "gseaResItem", function(object) return(object@geneIndices))
setMethod("gseaESprofile", "gseaResItem", function(object) return(object@esProfile))

setMethod("gseaCoreEnrichThr", "gseaResItem", function(object) return(object@coreEnrichThr))
setMethod("gseaCoreEnrichThr", "annoGseaRes", function(object) {
  res <- sapply(object, gseaCoreEnrichThr)
  names(res) <- gsName(object)
  return(res)
})


setMethod("gsDesc", "GeneSets", function(object, i) {
  res <- sapply(object, function(x) x$description)
  if(!missing(i)) res <- res[i]
  return(res)
})

setMethod("gsGenes", "annoGseaResItem", function(object) return(object@gsGenes))
setMethod("gsGenes", "annoGseaRes", function(object) {
  res <- lapply(object, gsGenes)
  names(res) <- gsName(object)
  return(res)
})
setMethod("gsGenes", "GeneSets", function(object, i) {
  res <- sapply(object, function(x) x$genes)
  if(!missing(i)) {
    res <- res[i]
    if(length(i)==1)
      res <- res[[1]]
  }
  return(res)
})

setMethod("gsGeneValues", "annoGseaResItem", function(object) return(object@gsGeneValues))
setMethod("gsGeneValues", "annoGseaRes", function(object) {
  res <- lapply(object, gsGeneValues)
  names(res) <- gsName(object)
  return(res)
})

setMethod("gseaCoreEnrichGenes", "annoGseaResItem", function(object) {
  gsGenes(object)[isGseaCoreEnrich(object)]
})
setMethod("gseaCoreEnrichGenes", "annoGseaRes", function(object) {
  res <- lapply(object, gseaCoreEnrichGenes)
  names(res) <- gsName(object)
  return(res)
})

setMethod("gsGenes<-", c("annoGseaResItem", "character"), function(object,value) {
  object@gsGenes <- value
  return(object)
})
setMethod("gsGeneValues<-", c("annoGseaResItem", "numeric"), function(object, value) {
  object@gsGeneValues <- value
  return(object)
})
setMethod("isGseaCoreEnrich", "annoGseaResItem", function(object) {
  nes <- gseaNES(object)
  thr <- gseaCoreEnrichThr(object)
  value <- gsGeneValues(object)
  if(nes<0) {
    value <= thr
  } else {
    value >= thr
  }
})

##setAs(from="list", to="gseaRes", def=function(from,to) {
##  haltifnot(all(sapply(from, function(x) is(x, "gseaResItem"))),
##            msg="Input list must be of gseaResItem objects")
##  res <- new("gseaRes", from)
##  return(res)
##})
setAs(from="list", to="annoGseaRes", def=function(from,to) {
  haltifnot(all(sapply(from, function(x) is(x, "annoGseaResItem"))),
            msg="Input list must be of annoGseaResItem objects")
  res <- new("annoGseaRes", from)
  return(res)
})
setAs(from="list", to="annoGseaResList", def=function(from,to) {
  haltifnot(all(sapply(from, function(x) is(x, "annoGseaRes"))),
            msg="Input list must be of annoGseaRes objects")
  res <- new("annoGseaResList", from)

  return(res)
})

##setMethod("gseaRes", "list", function(object) {
##  as(object, "gseaRes")
##})

setMethod("annoGseaRes", "list", function(object) {
  return(as(object, "annoGseaRes"))
})
##setMethod("[", "gseaRes", function(x, i) {
##  res <- callGeneric(x@.Data, i)
##  return(as(res, "gseaRes"))
##})
setMethod("[", "annoGseaRes", function(x, i,...) {
  if(all(is.character(i)))
    i <- match(i, gsName(x))
  res <- callGeneric(x@.Data, i)
  return(as(res, "annoGseaRes"))
})

setMethod("show", "GeneSets", function(object) {
  cat("[[", length(object), "GeneSets ]]", "\n")
  heads <- 1:pmin(3L, length(object))
  cat("--------------------\n")
  for(i in heads) {
    cat("name:", object[[i]]$name, "\n")
    cat("description:", object[[i]]$description, "\n")
    cat("genes: ", paste(head(object[[i]]$genes), collapse=","), ",...", "\n", sep="")
    cat("--------------------\n")
  }
  cat("...\n")
})

setMethod("show", "gseaResItem", function(object) {
  gInd <- gsGeneIndices(object)
  fmt <- "GeneSet \"%s\" [%d genes]\nES=%1.3f; NES=%1.3f; \
Nominal P-value(NP)=%1.3f; FDR=%1.3f; FWER=%1.3f\
Indices:%s\nEnrichment Score (ES) profile:%s\nCore enrichment threshold of input value:%1.3f\n"
  str <- sprintf(fmt,
                 gsName(object),
                 length(gInd),
                 gseaES(object),
                 gseaNES(object),
                 gseaNP(object),
                 gseaFDR(object),
                 gseaFWER(object),
                 paste(gInd, collapse=","),
                 paste(gseaESprofile(object), collapse=","),
                 gseaCoreEnrichThr(object))
  cat(str)
})
setMethod("show", "annoGseaResItem", function(object) {
  gInd <- gsGeneIndices(object)
  fmt <- "AnnotatedGeneSet \"%s\" [%d genes]\nES=%1.3f; NES=%1.3f; \
Nominal P-value(NP)=%1.3f; FDR=%1.3f; FWER=%1.3f\
Enrichment Score (ES) profile:%s\nCore enrichment threshold of input value:%1.4f\
GeneNames:%s\nGene input values:%s\n"
  str <- sprintf(fmt,
                 gsName(object),
                 length(gInd),
                 gseaES(object),
                 gseaNES(object),
                 gseaNP(object),
                 gseaFDR(object),
                 gseaFWER(object),
                 paste(gseaESprofile(object), collapse=","),
                 gseaCoreEnrichThr(object),
                 paste(gsGenes(object),collapse=","),
                 paste(gsGeneValues(object), collapse=","))
  cat(str)
})

setMethod("annoGseaResItem", "gseaResItem", function(object, genes, genevalues) {
  res <- as(object, "annoGseaResItem")
  if(!(length(genes)==length(genevalues) && length(genes)==length(gsGeneIndices(object))))
    stop(sprintf("genes (#=%d) and genevalues (#=%d) must be of the same length as the gsGeneIndices (#=%d)",
                 length(genes), length(genevalues), length(gsGeneIndices(object))))
  gsGenes(res) <- genes
  gsGeneValues(res) <- genevalues
  return(res)
})

setMethod("show", "annoGseaRes", function(object) {
  str <- sprintf("Annotated GSEA Results with %d gene sets\n",
                 length(object))
  cat(str)
})

## back compatibility
gsNames <- gsName
gsDescs <- gsDesc
