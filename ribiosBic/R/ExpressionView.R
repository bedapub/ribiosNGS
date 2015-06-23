mytoString <- function (x) {paste(x, collapse = " ")}
formatter <-function (x) 
{
    if (!is.na(x)) {
        if (abs(x) < 0.01) {
            res <- formatC(x, digits = 2, format = "e")
        }
        else {
            res <- formatC(x, digits = 2, format = "f")
        }
    }
    else {
        res <- formatC(0, digits = 2, format = "f")
    }
    res
}

biosISAGO <- function(modules, ann = annotation(modules), features = featureNames(modules), 
    hgCutoff = 0.05, correction = TRUE, correction.method = "holm") 
{
    library(paste(sep = "", ann, ".db"), character.only = TRUE)
    ENTREZ <- get(paste(sep = "", ann, "ENTREZID"))
    selectedEntrezIds <- getFeatureNames(modules)
    selectedEntrezIds <- lapply(selectedEntrezIds, function(x) unlist(AnnotationDbi::mget(x,ENTREZ,ifnotfound=NA)))
    selectedEntrezIds <- lapply(selectedEntrezIds, unique)
    entrezUniverse <- unique(unlist(AnnotationDbi::mget(features, ENTREZ,ifnotfound=NA)))
    paramsBP <- paramsCC <- paramsMF <- new("GOListHyperGParams", 
        geneIds = selectedEntrezIds, universeGeneIds = entrezUniverse, 
        annotation = ann, ontology = "BP", pvalueCutoff = hgCutoff, 
        conditional = FALSE, testDirection = "over", drive = TRUE)
    ontology(paramsCC) <- "CC"
    ontology(paramsMF) <- "MF"
    hgOverBP <- hyperGTest(paramsBP)
    hgOverCC <- hyperGTest(paramsCC)
    hgOverMF <- hyperGTest(paramsMF)
    res <- list(BP = hgOverBP, CC = hgOverCC, MF = hgOverMF)
    if (correction) {
        for (j in seq_along(res)) {
            for (i in seq_along(res[[j]]@reslist)) {
                res[[j]]@reslist[[i]]$Pvalue <- p.adjust(res[[j]]@reslist[[i]]$Pvalue, 
                  method = correction.method)
            }
        }
    }
    res
}

biosISAKEGG <- function (modules, ann = annotation(modules), features = featureNames(modules), 
    hgCutoff = 0.05, correction = TRUE, correction.method = "holm") 
{
    library(paste(sep = "", ann, ".db"), character.only = TRUE)
    library(Category)
    library(KEGG.db)
    ENTREZ <- get(paste(sep = "", ann, "ENTREZID"))
    selectedEntrezIds <- getFeatureNames(modules)
    selectedEntrezIds <- lapply(selectedEntrezIds, function(x) unlist(AnnotationDbi::mget(x,ENTREZ,ifnotfound=NA)))
    selectedEntrezIds <- lapply(selectedEntrezIds, unique)
    entrezUniverse <- unique(unlist(AnnotationDbi::mget(features, ENTREZ,ifnotfound=NA)))
    params <- try(new("KEGGListHyperGParams", geneIds = selectedEntrezIds, 
        universeGeneIds = entrezUniverse, annotation = ann, pvalueCutoff = hgCutoff, 
        testDirection = "over", drive = TRUE))
    hgOver <- hyperGTest(params)
    if (correction) {
        for (i in seq_along(hgOver@reslist)) {
            hgOver@reslist[[i]]$Pvalue <- p.adjust(hgOver@reslist[[i]]$Pvalue, 
                method = correction.method)
        }
    }
    hgOver
}

biosNormalize <- function (data, cutoff) 
{
    data.min <- min(data, na.rm = TRUE)
    data.max <- max(data, na.rm = TRUE)
    data.delta <- data.max - data.min
    if (data.min < 0 && data.max > 0) {
        data[data < 0] <- -data[data < 0]/data.min
        data[data > 0] <- data[data > 0]/data.max
    }
    else {
        data <- (data - data.min)/data.delta * 2 - 1
    }
    h <- hist(abs(data), plot = FALSE, breaks = 500)
    h <- h$counts
    total <- sum(h)
    for (i in 1:length(h)) {
        if (sum(h[1:i]) > cutoff * total) {
            break
        }
    }
    data <- data/(i/length(h))
    return(data)
}

base64cut <- function (x, at = 76) {
    .Call("EV_base64cut", as.character(x), as.integer(at), package = "igraph")
}

biosExportEV <- function (biclusters, eset, order = OrderEV(biclusters), filename = file.choose(), 
                            norm = c("sample", "feature", "raw", "x", "y"), cutoff = 0.95, 
                            description = NULL,

                            ...) {
  eisamodules <- as(biclusters, "ISAModules")
  eisamodules@rundata$annotation <- annotation(eset)
  eisamodules@rundata$prenormalize <- FALSE

  GO = biosISAGO(eisamodules)
  KEGG = biosISAKEGG(eisamodules)

  
  require(AnnotationDbi)
  require(GO.db)
  require(KEGG.db)
  require(affy)
  require(eisa)
  norm <- match.arg(norm)
  con <- file(filename, open = "w", blocking = TRUE)
  if (is.null(eisamodules@rundata$annotation)) {
    eisamodules@rundata$annotation <- annotation(eset)
  }
  if (is.null(eisamodules@rundata$prenormalize)) {
    eisamodules@rundata$prenormalize <- FALSE
  }
  geneMaps <- order$genes
  sampleMaps <- order$samples
  Genes <- featureNames(eisamodules)[geneMaps[[1]]]
  Samples <- sampleNames(eisamodules)[sampleMaps[[1]]]
  nGenes <- (dim(eisamodules))[1]
  nSamples <- (dim(eisamodules))[2]
  nEisamodules <- length(eisamodules)
  writeLines("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", 
             con)
  writeLines("<evf>", con)
  writeLines("\t<summary>", con)
  writeLines("\t\t<description>ExpressionView data file</description>", 
             con)
  writeLines("\t\t<version>1.1</version>", con)
  writeLines("\t\t<dataorigin>eisa</dataorigin>", con)
  writeLines(paste("\t\t<nmodules>", nEisamodules, "</nmodules>", 
                   sep = ""), con)
  writeLines(paste("\t\t<ngenes>", nGenes, "</ngenes>", 
                   sep = ""), con)
  writeLines(paste("\t\t<nsamples>", nSamples, "</nsamples>", 
                   sep = ""), con)
  writeLines("\t</summary>", con)
  go.table <- toTable(GOTERM)
  kegg.table <- toTable(KEGGPATHID2NAME)
  ann <- annotation(eset)
  library(paste(ann, sep = "", ".db"), character.only = TRUE)
  symbol.table <- toTable(get(paste(ann, "SYMBOL", sep = "")))
  entrez.table <- toTable(get(paste(ann, "ENTREZID", sep = "")))
  writeLines("", con)
  writeLines("\t<experimentdata>", con)
  writeLines(paste("\t\t<title>", eset@experimentData@title, 
                   "</title>", sep = ""), con)
  writeLines(paste("\t\t<name>", eset@experimentData@name, 
                   "</name>", sep = ""), con)
  writeLines(paste("\t\t<lab>", eset@experimentData@lab, 
                   "</lab>", sep = ""), con)
  writeLines(paste("\t\t<abstract>", eset@experimentData@abstract, 
                   "</abstract>", sep = ""), con)
  writeLines(paste("\t\t<url>", eset@experimentData@url, 
                   "</url>", sep = ""), con)
  writeLines(paste("\t\t<annotation>", eset@annotation, 
                   "</annotation>", sep = ""), con)
  organism <- get(paste(ann, "ORGANISM", sep = ""))
  writeLines(paste("\t\t<organism>", organism, "</organism>", 
                   sep = ""), con)
  writeLines("\t</experimentdata>", con)
  writeLines("", con)
  writeLines("\t<genes>", con)
  writeLines("\t\t<genetags>", con)
  writeLines("\t\t\t<id>#</id>", con)
    writeLines("\t\t\t<name>Name</name>", con)
  writeLines("\t\t\t<symbol>Symbol</symbol>", con)
  writeLines("\t\t\t<entrezid>EntrezID</entrezid>", con)
  writeLines("\t\t</genetags>", con)
  writeLines("", con)
  genemap <- match(Genes, symbol.table[, 1])
  genetext <- paste(sep = "", "\t\t<gene>\n\t\t\t<id>", 
                    seq_len(nGenes), "</id>\n\t\t\t<name>", Genes, "</name>\n\t\t\t<symbol>", 
                    symbol.table[genemap, 2], "</symbol>\n\t\t\t<entrezid>", 
                    entrez.table[genemap, 2], "</entrezid>\n\t\t</gene>")
  writeLines(genetext, con)
  writeLines("\t</genes>", con)
  writeLines("\t<samples>", con)
  writeLines("\t\t<sampletags>", con)
  writeLines("\t\t\t<id>#</id>", con)
    writeLines("\t\t\t<name>Name</name>", con)
  temp <- rownames(phenoData(eset)@varMetadata)
  temp <- gsub("[^[:alnum:]]", "_", temp)
  tempp <- phenoData(eset)@varMetadata[[1]]
  if (length(temp) >= 2) {
    for (i in 2:length(temp)) {
      writeLines(paste("\t\t\t<x name=\"", temp[i], 
                       "\">", temp[i], "</x>", sep = ""), con) ## changed from tempp[i] to temp[i]
    }
  }
  writeLines("\t\t</sampletags>", con)
  writeLines("", con)
  for (sample in 1:nSamples) {
    writeLines("\t\t<sample>", con)
    writeLines(paste("\t\t\t<id>", sample, "</id>", sep = ""), 
               con)
    writeLines(paste("\t\t\t<name>", Samples[sample], 
                     "</name>", sep = ""), con)
    if (dim(eset@phenoData@data)[2] != 0) {
      tempp <- eset@phenoData@data[sampleMaps[[1]][sample], 
                                   ]
      for (i in 2:length(temp)) {
        writeLines(paste("\t\t\t<x name=\"", temp[i], 
                         "\">", tempp[i], "</x>", sep = ""), con)
      }
    }
    writeLines("\t\t</sample>", con)
  }
  writeLines("\t</samples>", con)
  writeLines("", con)
  writeLines("\t<modules>", con)
  writeLines("\t\t<moduletags>", con)
  writeLines("\t\t\t<id>#</id>", con)
    writeLines("\t\t\t<name>Name</name>", con)
  if (dim(eisamodules@seeddata)[1] > 0) {
    temp <- colnames(eisamodules@seeddata)
    temp2 <- setdiff(temp, allowed)
    temp <- intersect(temp, allowed)
    tempp <- gsub("[^[:alnum:]]", "_", temp)
    if (length(temp) >= 1) {
      for (i in seq_len(length(temp))) {
        writeLines(paste("\t\t\t<", tempp[i], ">", 
                         temp[i], "</", tempp[i], ">", sep = ""), 
                   con)
      }
    }
    tempp <- gsub("[^[:alnum:]]", "_", temp2)
    if (length(temp2) >= 1) {
      for (i in seq_len(length(temp2))) {
        writeLines(paste("\t\t\t<x name=\"", tempp[i], 
                         "\">", temp2[i], "</x>", sep = ""), con)
      }
    }
  }
  writeLines("\t\t</moduletags>", con)
  writeLines("", con)
  writeLines("\t\t<gotags>", con)
  writeLines("\t\t\t<id>#</id>", con)
    writeLines("\t\t\t<go>GO</go>", con)
  writeLines("\t\t\t<term>Term</term>", con)
  writeLines("\t\t\t<ontology>Ontology</ontology>", con)
  writeLines("\t\t\t<pvalue>PValue</pvalue>", con)
  writeLines("\t\t\t<oddsratio>OddsRatio</oddsratio>", 
             con)
  writeLines("\t\t\t<expcount>ExpCount</expcount>", con)
  writeLines("\t\t\t<count>Count</count>", con)
  writeLines("\t\t\t<size>Size</size>", con)
  writeLines("\t\t</gotags>", con)
  writeLines("\t\t<keggtags>", con)
  writeLines("\t\t\t<id>#</id>", con)
    writeLines("\t\t\t<kegg>KEGG</kegg>", con)
  writeLines("\t\t\t<pathname>Path Name</pathname>", con)
  writeLines("\t\t\t<pvalue>PValue</pvalue>", con)
  writeLines("\t\t\t<oddsratio>OddsRatio</oddsratio>", 
             con)
  writeLines("\t\t\t<expcount>ExpCount</expcount>", con)
  writeLines("\t\t\t<count>Count</count>", con)
  writeLines("\t\t\t<size>Size</size>", con)
  writeLines("\t\t</keggtags>", con)
  writeLines("", con)
  for (module in 1:nEisamodules) {
    writeLines("\t\t<module>", con)
    writeLines(paste("\t\t\t<id>", module, "</id>", sep = ""), 
               con)
    writeLines(paste("\t\t\t<name>module ", module, "</name>", 
                     sep = ""), con)
    if (dim(eisamodules@seeddata)[1] > 0) {
      temp <- colnames(eisamodules@seeddata)
      temp2 <- setdiff(temp, allowed)
      temp <- intersect(temp, allowed)
      temp <- gsub("[^[:alnum:]]", "_", temp)
      if (length(temp) != 0) {
        tempp <- eisamodules@seeddata[module, ]
        for (i in seq_len(length(temp))) {
          value <- tempp[i]
          if (temp[i] == "rob" || temp[i] == "rob_limit") {
            value <- formatter(as.numeric(value))
          }
          writeLines(paste("\t\t\t<", temp[i], ">", 
                           value, "</", temp[i], ">", sep = ""), con)
        }
      }
      temp2 <- gsub("[^[:alnum:]]", "_", temp2)
      if (length(temp2) != 0) {
        temp2p <- eisamodules@seeddata[module, ]
        for (i in seq_len(length(temp2))) {
          value <- temp2p[i]
          writeLines(paste("\t\t\t<x name=\"", temp2[i], 
                           "\">", value, "</x>", sep = ""), con)
        }
      }
    }
    intersectingeisamodulesgenes = list()
    genes <- eisamodules@genes[, module][geneMaps[[1]]]
    for (modulep in 1:nEisamodules) {
      if (sum(genes * eisamodules@genes[, modulep][geneMaps[[1]]]) != 
          0 && module != modulep) {
        intersectingeisamodulesgenes <- append(intersectingeisamodulesgenes, 
                                              modulep)
      }
    }
    genesp <- match(as.vector(which(eisamodules@genes[, 
                                                     module] != 0)), geneMaps[[1]])[geneMaps[[module + 
                                                      1]]]
    writeLines(paste("\t\t\t<containedgenes>", mytoString(genesp), 
                     "</containedgenes>", sep = ""), con)
    scores <- as.array(as.real(genes[genesp]))
    scores <- apply(scores, 1, formatter)
    writeLines(paste("\t\t\t<genescores>", mytoString(scores), 
                     "</genescores>", sep = ""), con)
    intersectingeisamodulessamples = list()
    samples <- eisamodules@conditions[, module][sampleMaps[[1]]]
    for (modulep in 1:nEisamodules) {
      if (sum(samples * eisamodules@conditions[, modulep][sampleMaps[[1]]]) != 
          0 && module != modulep) {
        intersectingeisamodulessamples <- append(intersectingeisamodulessamples, 
                                                modulep)
      }
    }
    samplesp <- match(as.vector(which(eisamodules@conditions[, 
                                                            module] != 0)), sampleMaps[[1]])[sampleMaps[[module + 
                                                             1]]]
    writeLines(paste("\t\t\t<containedsamples>", mytoString(samplesp), 
                     "</containedsamples>", sep = ""), con)
    scores <- as.array(as.real(samples[samplesp]))
    scores <- apply(scores, 1, formatter)
    writeLines(paste("\t\t\t<samplescores>", mytoString(scores), 
                     "</samplescores>", sep = ""), con)
    intersectingeisamodules <- intersect(unique(intersectingeisamodulesgenes), 
                                        unique(intersectingeisamodulessamples))
    writeLines(paste("\t\t\t<intersectingmodules>", mytoString(intersectingeisamodules), 
                     "</intersectingmodules>", sep = ""), con)
    writeLines("", con)
    writeLines("\t\t\t<gos>", con)
    k <- 1
    for (i in 1:3) {
      s <- eisa::summary(GO[[i]])[[module]]
      if (dim(s)[1] > 0) {
        temp <- match(rownames(s), go.table[, 1])
        for (j in 1:dim(s)[1]) {
          writeLines("\t\t\t\t<go>", con)
          writeLines(paste("\t\t\t\t\t<id>", k, "</id>", 
                           sep = ""), con)
          k <- k + 1
          writeLines(paste("\t\t\t\t\t<go>", rownames(s)[j], 
                           "</go>", sep = ""), con)
          writeLines(paste("\t\t\t\t\t<term>", go.table[temp[j], 
                                                        3], "</term>", sep = ""), con)
          writeLines(paste("\t\t\t\t\t<ontology>", 
                           go.table[temp[j], 4], "</ontology>", sep = ""), 
                     con)
          writeLines(paste("\t\t\t\t\t<pvalue>", formatter(s[j, 
                                                             1]), "</pvalue>", sep = ""), con)
          writeLines(paste("\t\t\t\t\t<oddsratio>", 
                           formatter(s[j, 2]), "</oddsratio>", sep = ""), 
                     con)
          writeLines(paste("\t\t\t\t\t<expcount>", 
                           formatter(s[j, 3]), "</expcount>", sep = ""), 
                     con)
          writeLines(paste("\t\t\t\t\t<count>", s[j, 
                                                  4], "</count>", sep = ""), con)
          writeLines(paste("\t\t\t\t\t<size>", s[j, 
                                                 5], "</size>", sep = ""), con)
          writeLines("\t\t\t\t</go>", con)
        }
      }
    }
    writeLines("\t\t\t</gos>", con)
    writeLines("", con)
    writeLines("\t\t\t<keggs>", con)
    s <- eisa::summary(KEGG)[[module]]
    if (dim(s)[1] > 0) {
      temp <- match(rownames(s), kegg.table[, 1])
      for (j in 1:dim(s)[1]) {
        writeLines("\t\t\t\t<kegg>", con)
        writeLines(paste("\t\t\t\t\t<id>", j, "</id>", 
                         sep = ""), con)
        writeLines(paste("\t\t\t\t\t<kegg>", rownames(s)[j], 
                         "</kegg>", sep = ""), con)
        writeLines(paste("\t\t\t\t\t<pathname>", kegg.table[temp, 
                                                            2][j], "</pathname>", sep = ""), con)
        writeLines(paste("\t\t\t\t\t<pvalue>", formatter(s[j, 
                                                           1]), "</pvalue>", sep = ""), con)
        writeLines(paste("\t\t\t\t\t<oddsratio>", formatter(s[j, 
                                                              2]), "</oddsratio>", sep = ""), con)
        writeLines(paste("\t\t\t\t\t<expcount>", formatter(s[j, 
                                                             3]), "</expcount>", sep = ""), con)
        writeLines(paste("\t\t\t\t\t<count>", s[j, 
                                                4], "</count>", sep = ""), con)
        writeLines(paste("\t\t\t\t\t<size>", s[j, 5], 
                         "</size>", sep = ""), con)
        writeLines("\t\t\t\t</kegg>", con)
      }
    }
    writeLines("\t\t\t</keggs>", con)
    writeLines("\t\t</module>", con)
  }
  writeLines("\t</modules>", con)
  if (norm == "x") {
    norm <- "feature"
  }
  if (norm == "y") {
    norm <- "sample"
  }
  Data <- eisa:::select.eset(eset, eisamodules, norm)
  if (dim(Data)[1] == 0 || dim(Data)[2] == 0) {
    Data <- eisa:::select.eset(eset, eisamodules, "raw")
    message("using raw data.")
  }
  Data <- Data[geneMaps[[1]], sampleMaps[[1]]]
  Data <- as.vector(t(Data))
  Data <- biosNormalize(Data, cutoff)
  Data <- as.integer(round(Data * 100, 0))
  writeLines("", con)
  writeLines("\t<data>", con)
  temp <- base64encode(writeBin(Data, raw(), size = 1))
  cat(base64cut(temp), file = con)
  writeLines("\t</data>", con)
  writeLines("</evf>", con)
  close(con)
}
