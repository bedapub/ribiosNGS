


setMethod("RiboSeq", c("DGEList", "DGEList", "vector"), function(RNA, RPF, groups) {
  if(!is.factor(groups)) {
    message("coerencing groups as factors")
    groups <- factor(groups)
  }
  RNA$samples$group <- groups
  RPF$samples$group <- groups
  obj <- new("RiboSeq",
             RNA=RNA, RPF=RPF, groups=groups)
  return(obj)
})
setMethod("RiboSeq", c("matrix", "matrix", "vector"), function(RNA, RPF, groups) {
  if(!is.factor(groups)) {
    message("coerencing groups as factors")
    groups <- factor(groups)
  }
  haltifnot(ncol(RNA)==ncol(RPF),
            msg=sprintf("RNA file sample number (%d) does not equal RPF file sample number (%d)",
              ncol(RNA), ncol(RPF)))

  haltifnot(ncol(RNA)==length(groups),
            msg=sprintf("RNA file sample number (%d) does not equal group number (%d)",
              ncol(RNA), length(groups)))
  haltifnot(ncol(RPF)==length(groups),
            msg=sprintf("RPF file sample number (%d) does not equal group number (%d)",
              ncol(RNRPF), length(groups)))  
  crns <- munion(rownames(RNA), rownames(RPF))
  RNA <- RNA[match(rownames(RNA), crns),,drop=FALSE]
  RPF <- RPF[match(rownames(RPF), crns),,drop=FALSE]
  rownames(RNA) <- rownames(RPF) <- crns
  RNA.dge <- DGEList(counts=RNA,
                     group=groups)
  RPF.dge <- DGEList(counts=RPF,
                     group=groups)
  obj <- new("RiboSeq",
             RNA=RNA.dge, RPF=RPF.dge,
             groups=groups)
  return(obj)
})


## normalization

setMethod("normalize", "RiboSeq",
          function(object, method=c("TMM","RLE", "upperquartile", "none"), ...) {
            object@RNA <- calcNormFactors(object@RNA, method=method, ...)
            object@RPF <- calcNormFactors(object@RPF, method=method, ...)
            return(object)
          })

## extract cpm
setMethod("countRNA", "RiboSeq", function(object) object@RNA$counts)
setMethod("countRPF", "RiboSeq", function(object) object@RPF$counts)

setMethod("cpmRNA", "RiboSeq", function(object) cpm(object@RNA))
setMethod("cpmRPF", "RiboSeq", function(object) cpm(object@RPF))

## translationEfficiency
ctapply <- function(matrix, fac, FUN, ...) {
  stopifnot(ncol(matrix)==length(fac))
  do.call(cbind, tapply(1:ncol(matrix), fac, function(x) {
    do.call(FUN, list(matrix[,x, drop=FALSE]), ...)
  }))
}
ctsum <- function(matrix, fac, na.rm=TRUE) {
  ctapply(matrix, fac, function(x) rowSums(x, na.rm=TRUE))
}

setMethod("cpmRNAGroupSum", "RiboSeq", function(object)
          ctsum(cpmRNA(object), object@groups))


setMethod("cpmRPFGroupSum", "RiboSeq", function(object)
          ctsum(cpmRPF(object), object@groups))


narmRowSums <- function(x) rowSums(x, na.rm=TRUE)
## translation efficiency: RPF footprint divided by RNA footprint
translationEfficiency <- function(object, offset=0) {
  haltifnot(inherits(object, "RiboSeq"),
            msg="'object' must be a RiboSeq object")
  rnaSum <- cpmRNAGroupSum(object)
  rpfSum <- cpmRPFGroupSum(object)
  log2Ratios <- log2((rpfSum+offset)/(rnaSum+offset))
  return(log2Ratios)
}

## cpmFilter
minGroupSampleSize <- function(riboSeq) {
  groups <- riboSeq@groups
  return(min(table(groups)))
  
}
setMethod("cpmFilter", c("RiboSeq"), function(object) {
  cpm.rna <- cpmRNA(object)
  cpm.rpf <- cpmRPF(object)
  minGroup <- minGroupSampleSize(object)
  is.valid.rna <- apply(cpm.rna, 1, function(x) sum(x>=1, na.rm=TRUE)>= minGroup)
  is.valid.rpf <- apply(cpm.rpf, 1, function(x) sum(x>=1, na.rm=TRUE)>= minGroup)
  is.valid <- is.valid.rna & is.valid.rpf
  object@RNA <- object@RNA[is.valid,]
  object@RPF <- object@RPF[is.valid,]
  return(object)
})

## scatter plot
riboseq.panel <- function(x,y, subscripts, isSig, hline, vline, ...) {
  isBad <- is.na(x) | is.na(y) | is.infinite(x) | is.infinite(y)
  xfil <- x[!isBad]
  yfil <- y[!isBad]

  panel.smoothScatter(xfil,yfil,...)
  panel.abline(0,1, col="lightgray", lwd=1)
  if(!missing(hline))
    panel.abline(h=hline, lwd=1, col="black")
  if(!missing(vline))
    panel.abline(v=vline, lwd=1, col="black")
  
  if(!missing(isSig) && any(isSig[subscripts])) {
    isSubSig <- isSig[subscripts]
    xp <- x[isSubSig]
    yp <- y[isSubSig]
    panel.points(xp,yp, col="red", pch=16)
  }
}
xyplot.RiboSeq <- function(x,
                           xlab="RNA [cpm]",
                           ylab="RPF [cpm]",
                           ...) {
  cpm.rna <- cpmRNA(x)
  cpm.rpf <- cpmRPF(x)
  eset.rna.df <- matrix2longdf(cpm.rna, longdf.colnames=c("Gene", "Sample", "TotalRNA"))
  eset.rpf.df <- matrix2longdf(cpm.rpf, longdf.colnames=c("Gene", "Sample", "RPF"))
  
  eset.rna.df$SampleIndex <- match(eset.rna.df$Sample, colnames(exp.rna))
  eset.rpf.df$SampleIndex <- match(eset.rpf.df$Sample, colnames(exp.rpf))
  
  eset.rr.df <- merge(eset.rna.df,
                      eset.rpf.df,
                      by=c("Gene", "SampleIndex"))
  eset.rr.df$SampleIndex <- factor(eset.rr.df$SampleIndex)
  eset.rr.df$SampleLabel <- with(eset.rr.df, paste(Sample.y, "/", Sample.x, sep=""))
  rr.scales <- list(x=list(alternating=1L, tck=c(1,0), log=10),
                    y=list(alternating=1L, tck=c(1,0), log=10))
  xyplot(RPF ~ TotalRNA | SampleLabel, data=eset.rr.df,
         panel=riboseq.panel,
         scales=rr.scales, as.table=TRUE,
         xlab=xlab, ylab=ylab,
         ...)
}

## QC
plotRNAMDS <- function(riboSeq, main="RNA",...) {
  plotMDS(riboSeq@RNA, main=main,...)
}
plotRPFMDS <- function(riboSeq, main="RPF", ...) {
  plotMDS(riboSeq@RPF, main=main, ...)
}

## edgeR analysis
estimateDisps <- function(dgeList, design) {
  dgeList <- estimateGLMCommonDisp(dgeList, design=design)
  dgeList <- estimateGLMTrendedDisp(dgeList, design=design)
  dgeList <- estimateGLMTagwiseDisp(dgeList, design=design)
  return(dgeList)
}
doLRT <- function(dgeList, design, contrasts, ...) {
  if(!is.matrix(contrasts))
    stop("'contrasts' must be specified in a matrix")
  fit <- glmFit(dgeList, design)
  lrts <- apply(contrasts, 2, function(x) glmLRT(fit, coef=1, contrast=x))
  tags <- lapply(lrts, function(x) {
    tb <- topTags(x, n=nrow(dgeList$counts))$table
    tb$Gene <- rownames(tb)
    tb <- putColsFirst(tb, "Gene")
  })
  comb <- do.call(rbind, tags)
  comb$contrast <- rep(colnames(contrasts), sapply(tags, nrow))
  csplit <- strsplit(comb$contrast, "-") ## only one-way ANOVA is allowed
  contA <- sapply(csplit, "[[", 1L)
  contB <- sapply(csplit, "[[", 2L)
  comb$contA <- contA
  comb$contB <- contB
  return(comb)
}
doEdgeR <- function(riboSeq, design, contrasts) {
  riboSeq@RNA <- estimateDisps(riboSeq@RNA, design=design)
  riboSeq@RPF <- estimateDisps(riboSeq@RPF, design=design)

  rnaLRT <- doLRT(riboSeq@RNA, design=design, contrasts=contrasts)
  rpfLRT <- doLRT(riboSeq@RPF, design=design, contrasts=contrasts)

  diffTable <- merge(rnaLRT, rpfLRT,
                     by=c("Gene", "contrast", "contA", "contB"),
                     suffix=c(".RNA", ".RPF"))
  return(diffTable)
}

## Babel
summarizeBabel <- function(babelRes, type=c("combined", "between"), contrasts=NULL) {
  type <- match.arg(type)
  betweenLists <- babelRes[[type]]
  res <- do.call(rbind, betweenLists)
  contrast <- rep(names(betweenLists), sapply(betweenLists, nrow))
  contrast <- gsub("\\.vs\\.", "-", contrast)
  res$contrast <- contrast
  colnames(res)[colnames(res)=="P-value"] <- "Pvalue"
  if(type=="between")  { ## Babel has problem with factor levels, and therefore the mRNA logFC is not always correct
    csplit <- strsplit(res$contrast, "-")
    contA <- sapply(csplit, "[[", 1L)
    contB <- sapply(csplit, "[[", 2L)
    res$contA <- contA
    res$contB <- contB
    res2 <- res
    res2$Direction <- -res2$Direction
    res2$contA <- contB
    res2$contB <- contA
    res <- rbind(res, res2)
    res <- removeColumns(res, c("mRNA_logFC", "mRNA_FDR", "Pvalue", "contrast"))
  }
  rownames(res) <- NULL
  return(res)
}

doBabel <- function(riboSeq, contrasts, nreps) {
  exp.rna <- riboSeq@RNA$counts
  exp.rpf <- riboSeq@RPF$counts
  common.names <- paste(colnames(exp.rna),
                        colnames(exp.rpf), sep=".")
  colnames(exp.rna) <- colnames(exp.rpf) <- common.names
  babel.total <- babel(rna=exp.rna,
                       rp=exp.rpf,
                       group=as.character(riboSeq@groups),
                       nreps=nreps, min.rna=1)
  return(babel.total)
}

##summarizeBabelResult <- function(babel.result) {
##  babel.combined <- summarizeBabel(babel.result, type="combined", contrasts=contrasts)
##  babel.between <- summarizeBabel(babel.result, type="between", contrasts=contrasts)
##  res <- list(combined=babel.combined,
##              between=babel.between)
##  return(res)
##}

mergeBabelEdgeR <- function(babel.between, mRNA.RPF.diff, teRatios) {
  full.tbl <- merge(mRNA.RPF.diff, babel.between,
                    by=c("Gene", "contA", "contB"),
                    suffix=c(".edgeR", ".babel"))
  colnames(full.tbl)[colnames(full.tbl)=="FDR"] <- "FDR.babel"
  stopifnot(all(full.tbl$contA %in% colnames(teRatios)) && all(full.tbl$contB %in% colnames(teRatios)))

  te.df <- matrix2longdf(teRatios, longdf.colnames=c("Gene", "group", "log2TE"))
  contA.ratio <- merge(te.df, full.tbl[,c("Gene", "contA")],
                       by.x=c("Gene","group"), by.y=c("Gene", "contA"))
  contB.ratio <- merge(te.df, full.tbl[,c("Gene", "contB")],
                       by.x=c("Gene","group"), by.y=c("Gene", "contB"))
  contRatio <- unique(merge(contA.ratio, contB.ratio,
                            by="Gene", suffix=c(".contA", ".contB")))
  contRatio$log2TE.diff <- contRatio$log2TE.contA - contRatio$log2TE.contB
  res <- merge(full.tbl, contRatio,
               by.x=c("Gene", "contA", "contB"),
               by.y=c("Gene", "group.contA", "group.contB"))
  res$babelScore <- -log10(res$FDR.babel) * sign(res$log2TE.diff)
  sres <- res[,c("Gene", "contrast", "contA", "contB",
                "logFC.RNA", "logCPM.RNA", "PValue.RNA", "FDR.RNA",
                "logFC.RPF", "logCPM.RPF", "PValue.RPF", "FDR.RPF",
                 "log2TE.contA", "log2TE.contB",
                 "log2TE.diff", "FDR.babel", "babelScore")]
  return(sres)
}

## IO
contrastName2readable <- function(contrast, prefix=NULL, suffix="txt", sep=".") {
  if(missing(prefix) || is.null(prefix) || is.na(prefix))
    prefix <- ""
  if(missing(suffix) || is.null(suffix) || is.na(suffix))
    suffix <- ""
  cont <- gsub("-", ".vs.", contrast)
  prefix <- trim(prefix, right=sep)
  suffix <- trim(suffix, left=sep)
  if(prefix!="") cont <- paste(prefix, cont, sep=sep)
  if(suffix!="") cont <- paste(cont, suffix, sep=sep)
  return(cont)
}
babelRnks <- function(babel.full, type=c("logFC.RNA", "logFC.RPF", "log2TE.diff", "babelScore")) {
  type <- match.arg(type)
  df <- babel.full[, c("Gene", type)]
  res <- split(df, babel.full$contrast)
  res <- lapply(res, function(x) x[!is.na(x[,2]) & !is.nan(x[,2]),])
  names(res) <- contrastName2readable(names(res), prefix=type, suffix=".rnk")
  return(res)
}
babelAllRnks <- function(babel.full) {
  totalRNA.rnks <- babelRnks(babel.full, type="logFC.RNA")
  RPF.rnks <- babelRnks(babel.full, type="logFC.RPF")
  TEdiff.rnks <- babelRnks(babel.full, type="log2TE.diff")
  babelScore.rnks <- babelRnks(babel.full, type="babelScore")
  c(totalRNA.rnks, RPF.rnks, TEdiff.rnks, babelScore.rnks)
}

## helper function
refBoxplot <- function(x, xlab=NULL, ylab=NULL, main=NULL, hline=NULL, vline=NULL,
                       ...,
                       ref.lwd=2, ref.col="lightgray", ref.lty=1) {
  boxplot(x,xlab="", ylab="", main="", ...)
  if(!is.null(hline))
    abline(h=hline, lwd=ref.lwd, col=ref.col, lty=ref.lty)
  if(!is.null(vline))
    abline(v=vline, lwd=ref.lwd, col=ref.col, lty=ref.lty)
  boxplot(x, xlab=xlab, ylab=ylab, main=main, add=TRUE,...)
}

write.tableList <- function(list, files, ...) {
  for(i in seq(along=list))
    write.table(list[[i]], files[[i]], ...)
}

setMethod("featureNames", "RiboSeq", function(object) {
  rownames(object@RNA)
})

gseaDataDir <- function() {
  return("/DATA/bi/httpd_8080/htdoc/apps/gsea")
}
gseaGeneSetDir <- function() {
  return(file.path(gseaDataDir(),
                   "genesets"))
}
listGseaGeneSets <- function() {
  dir(gseaGeneSetDir(), pattern="*.gmt")
}
gseaGeneSetFile <- function(file.name) {
  file <- file.path(gseaGeneSetDir(),
                    file.name)
  if(!file.exists(file)) {
    stop(file.name, "not found!")
  }
  return(file)
}
readGseaGeneSet <- function(file.name) {
  read_gmt_list(gseaGeneSetFile(file.name))
}


geneSetMedianTest <- function (index, statistics, alternative = "mixed", type = "auto", 
    ranks.only = TRUE, nsim = 9999) 
{
    alternative <- match.arg(alternative, c("mixed", "either", 
        "down", "up", "less", "greater", "two.sided"))
    if (alternative == "two.sided") 
        alternative <- "either"
    if (alternative == "less") 
        alternative <- "down"
    if (alternative == "greater") 
        alternative <- "up"
    type <- match.arg(tolower(type), c("auto", "t", "f"))
    allsamesign <- all(statistics >= 0) || all(statistics <= 
        0)
    if (type == "auto") {
        if (allsamesign) 
            type <- "f"
        else type <- "t"
    }
    if (type == "f" & alternative != "mixed") 
        stop("Only alternative=\"mixed\" is possible with F-like statistics.")
    if (alternative == "mixed") 
        statistics <- abs(statistics)
    if (alternative == "down") {
        statistics <- -statistics
        alternative <- "up"
    }
    if (ranks.only) {
        pvalues <- rankSumTestWithCorrelation(index = index, 
            statistics = statistics, df = Inf)
        p.value <- switch(alternative, down = pvalues["less"], 
            up = pvalues["greater"], either = 2 * min(pvalues), 
            mixed = pvalues["greater"])
    }
    else {
        ssel <- statistics[index]
        ssel <- ssel[!is.na(ssel)]
        nsel <- length(ssel)
        if (nsel == 0) 
            return(1)
        stat <- statistics[!is.na(statistics)]
        msel <- median(ssel)
        if (alternative == "either") 
            posstat <- abs
        else posstat <- function(x) x
        msel <- posstat(msel)
        ntail <- 1
        for (i in 1:nsim) if (posstat(median(sample(stat, nsel))) >= 
            msel) 
            ntail <- ntail + 1
        p.value <- ntail/(nsim + 1)
    }
    as.vector(p.value)
}



fisher.method <- function(p) {
  Xsq <- -2*sum(log(p))
  p.val <- pchisq(Xsq, df = 2*length(p),lower.tail=FALSE)
  return(c(Xsq = Xsq, p.value = p.val))
}

pair.fisher.method <- function(p1, p2) {
  stopifnot(length(p1)==length(p2))
  res <- data.frame(t(sapply(seq(along=p1), function(i)
                             fisher.method(c(p1[i], p2[i])))))
}

finite.t.test.pvalue <- function(x,y, ...) {
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if(length(x)<2 || length(y)<2)
    return(NA)
  return(t.test(x, y, ...)$p.value)
}
gage <- function(rnks, ind.list, alternative="two.sided") {
  sapply(ind.list, function(x) {
    finite.t.test.pvalue(rnks[x], rnks[-x], alternative=alternative)
  })
}
## doGeneSetTest using a simple data structure (a 2-column data frame of gene names and statistics)
doGeneSetTest <- function(rnk, anno,
                          gmt, minGene=NULL, maxGene=NULL, nsim=9999, ...) {
  stopifnot(is.data.frame(rnk) & ncol(rnk)>=2 & is.numeric(rnk[,2]))
  if(is.null(minGene)) minGene <- 0
  if(is.null(maxGene)) maxGene <- Inf
  sorted.anno <- matchColumn(rnk[,1L], anno, 0L)
  sorted.gs <- sorted.anno$GeneSymbol
  all.index <- lapply(gmt, function(x) na.omit(match(x$genes,
                                                     sorted.gs)))

  all.size <- sapply(gmt, function(x) length(x$genes)) 
  all.effsize <- sapply(all.index, function(x) length(x))
  hasGenes <- all.effsize >= minGene & all.effsize <= maxGene
  sub.index <- all.index[hasGenes]
  ## gs <- sapply(sub.index, function(x) geneSetTest(x, rnk[,2L], alternative="either"))
  ## gs.sim <- sapply(sub.index, function(x) geneSetMedianTest(x, rnk[,2L], alternative="either", ranks.only=FALSE, nsim=nsim))
  gs.sim <- gage(rnk[,2], ind.list=sub.index, alternative="two.sided")
  gs.wmw <- BioQC::wmwTest(rnk[,2], ind.list=sub.index, alternative="two.sided")
  gs.wmw.up <- BioQC::wmwTest(rnk[,2], ind.list=sub.index, alternative="greater")
  gs.wmw.down <- BioQC::wmwTest(rnk[,2], ind.list=sub.index, alternative="less")
  gs.comb <- pair.fisher.method(gs.sim, gs.wmw)$p.value
  gs.median <- sapply(sub.index, function(x) median(rnk[x,2]))
  gs.direction <- ifelse(gs.wmw.up<=gs.wmw.down, "up", "down")
  ## wmw bootstrapping (almost identical to gs.wmw)
  ##sub.index.len <- sapply(sub.index, length)
  ##sub.index.ulen <- unique(sub.index.len)
  ##Nboot <- 99999
  ##sub.index.boot <- lapply(seq(sub.index.ulen),
  ##                         function(i)
  ##                         lapply(1:Nboot, function(x) {
  ##                           sample(1:nrow(rnk), sub.index.ulen[i], replace=TRUE)
  ##                         }))
  ##sub.index.boot.wmw <- lapply(sub.index.boot, function(x)
  ##                             wmwTest(rnk[,2], x, alternative="two.sided"))
  ##gs.wmw.boot.ind <- match(sub.index.len, sub.index.ulen)
  ##gs.wmw.sim <- sapply(seq(along=sub.index),
  ##                     function(i) mean(gs.wmw[i] >= sub.index.boot.wmw[[gs.wmw.boot.ind[i]]]))
  res.raw <- data.frame(geneset=names(sub.index),
                        median=gs.median,
                        direction=gs.direction,
                        p.sim=gs.sim,
                        p.wmw=gs.wmw,
                        p.comb=gs.comb,
                        row.names=NULL)
  gs.info <- data.frame(geneset=names(all.index),
                        size=all.size,
                        effective.size=all.effsize, row.names=NULL)
  res <- merge(gs.info, res.raw, by="geneset", all.x=TRUE)
  return(res)
}

doGeneSetTests <- function(rnks, anno,
                           gmt, minGene=NULL, maxGene=NULL, nsim=9999, ...) {
  resl <- lapply(rnks, function(rnk) {
    doGeneSetTest(rnk, anno=anno,
                  gmt=gmt, minGene=minGene, maxGene=maxGene, nsim=nsim,...)
  })
  res <- cbind(contrast=rep(names(rnks), sapply(resl, nrow)),
               do.call(rbind, resl))
  rownames(res) <- NULL
  return(res)
}

## riboseq analysis object



pngFilename <- function(filename, full.names=FALSE) {
  res <- file.path(dirname(filename),
                   paste(basefilename(filename), ".png", sep=""))
  if(!full.names)
    res <- basename(res)
  return(res)
}
setMethod("convertPDF2PNG", "riboSeqAnalysisObject",
          function(object) {
            outd <- object@outdir
            plots <- grep("^plot", slotNames(object),v=TRUE)
            plotBaseFiles <- sapply(plots, function(x) slot(object, x))

            plotFiles <- file.path(outd, plotBaseFiles)
            assertFile(plotFiles)

            outfile <- pngFilename(plotBaseFiles)
            pdf2png(plotFiles, outdir=outd, outfile=outfile, wait=TRUE)
            return(outfile)
          })

setMethod("setRnks", c("riboSeqAnalysisObject"),
          function(object, names) {
            object@rnkNames <- names
            object@file.rnks <- sprintf("riboseq-%s", names(babel.rnks))
            return(object)
          })

getImageHTML <- function(object,
                         slotName="plot.MDS",
                         desc="Between-sample relationship represented with MDS plot") {
  pdfFile <- slot(object, slotName)
  res <- paste("<div>",desc, " (<a href='",
               pdfFile,
               "'/>Download PDF</a>)",
               "<img src='",
               basename(pngFilename(pdfFile)),
               "'  style='width:80%;display:block;margin-left:auto;margin-right:auto;'/> ",
               "</div>",
               sep="")
  return(res)
}
getFileHTML <- function(object,
                        slotName="plot.MDS",
                        desc="Between-sample relationship represented with MDS plot") {
  file <- slot(object, slotName)
  sepc <- rep(": ", length(file))
  sepc[nchar(desc)==0] <- ""
  res <- paste(desc, sepc, "<a href='",
               file,
               "'>",
               file,
               "</a>",sep="")
  return(res)
}
getFile <- function(object, slot, full.names=TRUE) {
  fn <- slot(object, name=slot)
  if(full.names) {
    return(file.path(object@outdir, fn))
  } else {
    return(fn)
  }
}
cpmBoxplotFile <- function(object) {
  getFile(object, "plot.cpmBoxplot", full.names=TRUE)
}
exprsScatterFile <- function(object) {
  getFile(object, "plot.exprsScatter", full.names=TRUE)
}
mdsFile <- function(object) {
  getFile(object, "plot.MDS", full.names=TRUE)
}
teBoxplotFile <- function(object) {
  getFile(object, "plot.teBoxplot", full.names=TRUE)
}
logFCscatterFile <- function(object) {
  getFile(object, "plot.logFCscatter", full.names=TRUE)
}
babelVolcanoFile <- function(object) {
  getFile(object, "plot.babelVolcano", full.names=TRUE)
}

upstreamFile <- function(object) {
  getFile(object, "file.upstream", full.names=TRUE)
}

functionFile <- function(object) {
  getFile(object, "file.function", full.names=TRUE)
}

pathwayFile <- function(object) {
  getFile(object, "file.pathway", full.names=TRUE)
}
rnkFiles <- function(object) {
  getFile(object, "file.rnks", full.names=TRUE)
}
fullTableFile <- function(object) {
  getFile(object, "file.fulltable", full.names=TRUE)
}
rspaceFile <- function(object) {
  getFile(object, "file.rspace", full.names=TRUE)
}

writeHTML <- function(analysisObject) {
  convertPDF2PNG(analysisObject)
  
  header <- "<header> \
  <title>RiboSeq Analysis Results</title> \
  </header>"
  
  title <- "<h1>RiboSeq Analysis Results</h1>"

  groups <- paste("<ol>",
                  paste("<li>", analysisObject@groups, "</li>",collapse=""),
                  "</ol>")
  levels <- paste("<ol>",
                  paste("<li>", levels(analysisObject@groups), "</li>",collapse=""),
                  "</ol>")
  contrasts <- paste("<ol>",
                  paste("<li>", analysisObject@contrasts, "</li>",collapse=""),
                  "</ol>")

  input <- sprintf("<h2>Input parameters</h2>\
  <ul> \
  <li>RNA file: %d features, %d samples</li> \
  <li>RPF file: %d features, %d samples</li> \
  <li>Combined data set: %d features, %d samples</li> \
  <li>Filtered data set: %d features, %d samples</li> \
  <li>groups: %s</li> \
  <li>levels: %s</li> \
  <li>contrasts: %s</li> \
  <li>Babel bootstrapping: %d</li> \
  </ul>",
                   analysisObject@nFeature.RNA.raw, analysisObject@nSample.RNA.raw,
                   analysisObject@nFeature.RPF.raw, analysisObject@nSample.RPF.raw,
                   analysisObject@nFeature.comb.raw, analysisObject@nSample.comb.raw,
                   analysisObject@nFeature.filter, analysisObject@nSample.filter,
                   groups,
                   levels,
                   contrasts,
                   analysisObject@nBootstrap)

  vis <- sprintf("<h2>Visualizations</h2> \
  <h3>Quality control</h3> \
  %s \
  %s \
  %s \
  %s \
  %s \
  %s",
                 getImageHTML(analysisObject, slotName="plot.cpmBoxplot", desc="Between-sample normlization"),
                 getImageHTML(analysisObject, slotName="plot.exprsScatter", desc="Scatter plot between transcription and translation"),
                 getImageHTML(analysisObject, slotName="plot.MDS", desc="Between-sample relationships visualized with multi-dimensional scaling (MDS) plot"),
                 getImageHTML(analysisObject, slotName="plot.teBoxplot", desc="Translational efficiency boxplots"),
                 getImageHTML(analysisObject, slotName="plot.logFCscatter", desc="Differential expression of RNA and RPF using edgeR"),
                 getImageHTML(analysisObject, slotName="plot.babelVolcano", desc="Volcano plot of differential translational efficiency detected by Babel"))

  rnkTrunk <- paste("<ul>",
                    getFileHTML(analysisObject, "file.rnks", desc=""),
                    "</ul>", collapse="\n")
  
  download <- sprintf("<h2>Download results</h2> \
  <h2>Differential expression and translation</h2> \
  <ul> \
  <li>%s</li>\
  <li>%s</li>\
  <li>rnk files for GSEA-like analysis: %s</li>\
  </ul> \

  <h2>Preliminary enrichment analysis results</h2> \
  <ul> \
  <li>%s</li> \
  <li>%s</li> \
  <li>%s</li> \
  </ul>",
                      getFileHTML(analysisObject, "file.fulltable", desc="A tab-delimited file of babel and edgeR analysis results"),
                      getFileHTML(analysisObject, "file.rspace", desc="R work space"),
                      rnkTrunk,
                      getFileHTML(analysisObject, "file.pathway", desc="Pathway analysis results"),
                      getFileHTML(analysisObject, "file.function", desc="Functional analysis results"),
                      getFileHTML(analysisObject, "file.upstream", desc="Upstream analysis results"))
  
  if(nchar(input)==0)  stop("Internal error: input length 0")
  if(nchar(vis)==0)  stop("Internal error: vis length 0")
  if(nchar(download)==0)  stop("Internal error: download length 0")
  res <- paste("<html>",
               header,
               "<body>",
               title,
               input,
               vis,
               download,
               "</body></html>")

  writeLines(res, file.path(analysisObject@outdir, analysisObject@file.indexHTML))
  return(invisible(res))
}
