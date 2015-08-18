setMethod("[","GeneSets", function(x, i, ...) {
              x@.Data <- x@.Data[i]
              return(x)
          })

setMethod("gsCategory", "GeneSet", function(object) return(object@category))
setMethod("gsCategory", "GeneSets", function(object) return(sapply(object@.Data, gsCategory)))

setMethod("gsName", "GeneSet", function(object) return(object@name))
setMethod("gsName", "gseaResItem", function(object) return(object@geneset))
setMethod("gsName", "annoGseaRes", function(object) sapply(object, gsName))
setMethod("gsName", "GeneSets", function(object, i) {
  res <- sapply(object, function(x) x$name)
  if(!missing(i) && !is.null(i)) res <- res[i]
  return(res)
})
setMethod("gsName", "FisherResult", function(object) object@gsName)

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

setMethod("gsGenes", "GeneSet", function(object) return(object@genes))
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
gseaLeadingEdgeGenes <- gseaCoreEnrichGenes

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
  categories <- gsCategory(object)
  cateTbl <- table(categories)
  categoryTerm <- ifelse(length(cateTbl)>1, "categories", "category")
  cat("A GeneSet object\n")
  cat("  Unique ", categoryTerm," (", length(cateTbl), "):\n", sep="")
  cat(paste("    [", seq(along=cateTbl), "] ",
            names(cateTbl), " (", cateTbl, ")", collapse="\n",
            sep=""),
      "\n",sep="")
  cat("  Gene sets (", length(object), "):\n",sep="")
  heads <- 1:pmin(3L, length(object))
  for(i in heads) {
    cat("    [", i, "] ", object[[i]]@name, "\n",sep="")
    cat("        description:", object[[i]]@desc, "\n")
    cat("        genes : ", paste(head(object[[i]]@genes), collapse=","), ",...", "\n", sep="")
  }
  cat("...\n")
})

setMethod("show", "GeneSetsList", function(object) {
  nl <- length(object)
  cat("[[A collection of ", nl, " GeneSets]]\n", sep="")
  for(i in nl) {
    cat("--------------------\n")
    cat("GeneSets:", object[[i]]@name, "\n")
    cat("Number of gene sets: ", length(object[[i]]), "\n")
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

## extending functions
gseaScore <- function(x, type=c("fdr", "p", "fwer")) {
  type <- match.arg(type)
  if(type=="fdr") {
    val <- gseaFDR(x)
  } else if (type=="p") {
    val <- gseaNP(x)
  } else if (type=="fwer") {
    val <- gseaFWER(x)
  }
  val[val==0] <- min(val[val!=0], na.rm=TRUE)
  res <- -log10(val) * sign(gseaES(x))
  return(res)
}

gseaScores <- function(..., names=NULL, type=c("fdr", "p", "fwer")) {
  ll <- list(...)
  scores <- lapply(ll, gseaScore, type=type)
  setnames <- munion(lapply(scores, names))
  res <- as.data.frame(sapply(scores, function(x) x[match(setnames, names(x))]))
  rownames(res) <- setnames
  if(!is.null(names)) {
    haltifnot(length(names)==length(ll), msg="names length must match the input list")
    colnames(res) <- names
  }
  return(res)
}

## back compatibility
gsNames <- gsName
gsDescs <- gsDesc


##----------------------------------------##
## Fisher's exact test
##----------------------------------------##
setMethod("gsEffSize", "FisherResult", function(object) return(object@gsEffSize))
setMethod("hits", "FisherResult", function(object) return(object@hits))

setMethod("gsCategory", "FisherResult", function(object) return(object@gsCategory))
setMethod("gsCategory", "FisherResultList", function(object) sapply(object@.Data, gsCategory))


setMethod("as.data.frame", "FisherResultList", function(x, row.names) {
              categories <- sapply(x, gsCategory)
              genesets <- sapply(x, gsName)
              ps <- sapply(x, pValue)
              fdrs <- sapply(x, fdrValue)
              hits <- lapply(x, hits)
              hitCounts <- sapply(hits, length)
              gsEffSize <- sapply(x, gsEffSize)
              inputSize <- length(x@input)
              universeSize <- length(x@universe)
              hitPrint <- sapply(hits, paste, collapse=",")
              data.frame(Category=categories,
                         GeneSet=genesets,
                         Pvalue=ps,
                         FDRvalue=fdrs,
                         HitCount=hitCounts,
                         InputSize=inputSize,
                         GeneSetEffectiveSize=gsEffSize,
                         UniverseSize=universeSize,
                         Hits=hitPrint,
                         row.names=row.names)
          })


##----------------------------------------##
## Fisher's exact test
##----------------------------------------##
setMethod("gsName", "FisherResultList", function(object,...) {
              sapply(object, function(x) x@gsName)
          })

setMethod("[[", c("FisherResultList", "numeric"), function(x, i) {
              return(x@.Data[[i]])
          })
setMethod("[[", c("FisherResultList", "character"), function(x, i) {
              which <- match(i, gsName(x))
              return(x[[which]])
          })

setMethod("[", c("FisherResultList", "numeric", "missing", "missing"), function(x, i,j,drop) {
              x@.Data <- x@.Data[i]
              return(x)
          })
setMethod("[", c("FisherResultList", "character", "missing", "missing"), function(x, i,j, drop) {
              which <- match(i, gsName(x))
              x@.Data <- x@.Data[which]
              return(x)
          })
setMethod("[", c("FisherResultList", "character", "character", "missing"),
          function(x, i,j, drop) {
              isCategory <- gsCategory(x) %in% i
              isName <- gsName(x) %in% j
              isSel <- isCategory & isName
              if(sum(isSel)==1) {
                  return(x@.Data[[which(isSel)]])
              } else if (sum(isSel)>1) {
                  return(x@.Data[isSel])
              } else {
                  stop(sprintf("No element found for category %s and gene set %s!\n",
                               i, j))
              }
          })
setMethod("[", c("FisherResultList", "character", "missing", "missing"),
          function(x, i,j, drop) {
              isCategory <- gsCategory(x) %in% i
              x@.Data <- x@.Data[isCategory]
              return(x)
          })
setMethod("[", c("FisherResultList", "missing", "character", "missing"),
          function(x, i,j, drop) {
              isName <- gsName(x) %in% j
              x@.Data <- x@.Data[isName]
              return(x)
          })

setMethod("hits", "FisherResult", function(object) {
              object@hits
          })
setMethod("hits", "FisherResultList", function(object, geneset) {
              if(missing(geneset)) {
                  res <- lapply(object, function(x) x@hits)
              } else {
                  res <- genes(object[[geneset]])
              }
              return(res)
          })
setMethod("pValue", "FisherResult", function(object) {return(object@p)})
setMethod("pValue", "FisherResultList", function(object, ind, ...) {
              res <- sapply(object@.Data, pValue)
              if(!missing(ind)) {
                  return(res[ind])
              } else {
                  return(res)
              }
    
          })
setMethod("fdrValue", "FisherResult", function(object) {return(object@fdr)})
setMethod("fdrValue", "FisherResultList", function(object, ind, ...) {
              res <- sapply(object@.Data, fdrValue)
              if(!missing(ind)) {
                  return(res[ind])
              } else {
                  return(res)
              }
          })
setMethod("minPValue", "FisherResultList", function(object,...) {
              min(pValue(object))
          })
setMethod("minFdrValue", "FisherResultList", function(object,...) {
              min(fdrValue(object))
          })
setMethod("isSigGeneSet", c("FisherResultList", "numeric"),function(object,fdr) {
              fdrValue(object)<fdr
          })
setMethod("sigGeneSet", c("FisherResultList", "numeric"),function(object,fdr) {
              gsName(object)[isSigGeneSet(object, fdr)]
          })
setMethod("sigGeneSetTable", c("FisherResultList", "numeric"),function(object,fdr,...) {
              as.data.frame(obj[isSigGeneSet(object, fdr)])
          })
setMethod("topGeneSetTable", c("FisherResultList", "numeric"),function(object,N,...) {
              ps <- pValue(object)
              pOrd <- order(ps, decreasing=FALSE)[1:pmin(N, length(ps))]
              sub <- object[pOrd]
              return(as.data.frame(sub))
          })
setMethod("topOrSigGeneSetTable", c("FisherResultList", "numeric", "numeric"), function(object, N, fdr) {
              fdrV <- fdrValue(object)
              N <- pmax(N, sum(fdrV<fdr))
              return(topGeneSetTable(object, N, fdr))
              
          })
setMethod("topOrSigGeneSetTable", c("FisherResultList", "numeric", "missing"), function(object, N, fdr) {
              topOrSigGeneSetTable(object, N, 0.05)
          })
setMethod("topOrSigGeneSetTable", c("FisherResultList", "missing", "missing"), function(object, N, fdr) {
              topOrSigGeneSetTable(object, 10, 0.05)
          })

setMethod("print", "FisherResult", function(x, ...) {
              if(!is.na(gsCategory(x)))
                  cat("Category:", gsCategory(x), "\n")
              if(!is.na(gsName(x)))
                  cat("Name:", gsName(x), "\n")
              cat("Gene set size:", gsEffSize(x), "\n")
              cat(sprintf("Hits (%d):", length(hits(x))),
                  paste(hits(x), collapse=","), "\n")
              cat("Fisher's exact p value:", pValue(x), "\n")
              cat("BH FDR value:", fdrValue(x), "\n")
          })
setMethod("show", "FisherResult", function(object) {
              print(object)
          })
setMethod("print", "FisherResultList", function(x,...) {
              cat("--- One-sided Fisher's exact tests for gene sets ---\n")
              cat(sprintf("Total input genes: %d\n", length(x@input)))
              cat(sprintf("Gene universe: %d\n", length(x@universe)))
              cat(sprintf("Total gene sets: %d\n", length(x)))
              cat(sprintf("Minimal P-value: %e\n", minPValue(x)))
              cat(sprintf("Minimal FDR-value: %e\n", minFdrValue(x)))
          })

setMethod("show", "FisherResultList", function(object) {
              print(object)
          })

setMethod("gsSize", "GeneSet", function(object) {
              return(length(object@genes))
          })
setMethod("gsSize", "GeneSets", function(object) {
              return(sapply(object@.Data, gsSize))
          })
setMethod("filterBySize",
          c("GeneSets", "ANY", "ANY"),
          function(object, min, max) {
              sizes <- gsSize(object)
              sel <- rep(TRUE, length(sizes))
              if(!missing(min)) {
                  min <- as.numeric(min)
                  if(!is.na(min)) {
                      sel <- sel & sizes >= min
                  }
              }
              if(!missing(max)) {
                  max <- as.numeric(max)
                  if(!is.na(max)) {
                      sel <- sel & sizes <= max
                  }
              }
              object@.Data <- object@.Data[sel]
              return(object)
          })
