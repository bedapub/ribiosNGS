#' Fisher's method to combine multiple p-values
#' @param p Numeric vector, p values to be combined
#' @param returnValidP Logical, whether the valid p-values used should be returned as part of the list
#' @return A list of following elements
#' \enumerate{
#' \item chisq: Chi-square statistic
#' \item df: Degree of freedom (which is twice the count of the valid p-values used for calculation)
#' \item p: p-value
#' \item validp (optional): valid p-values used for the calculation
#' }
#' The function returns the combined p-value using the sum of logs (Fisher's) method
#' \note The function was adapted from metap::sumlog
#' 
#' \examples
#' ps <- c(0.05, 0.75)
#' fishersMethod(ps)
fishersMethod <- function(p, returnValidp=FALSE) {
  keep <- !is.na(p) & p>0 & p<=1
  if(!all(keep)) {
    warning("P-values outside (0,1] omitted")
  }
  lnp <- log(p[keep])
  chisq <- (-2) * sum(lnp)
  df <- 2 * length(lnp)
  if (sum(keep) == 1) {
    warning("Only one p-value provided. The original p value is returned.")
    res <- list(chisq=chisq, df=df,
                p=p[keep])
  } else if (!any(keep)) {
    res <- list(chisq=NA, df=NA, p=NA)  
  } else {
    res <- list(chisq = chisq, df = df, 
                p = pchisq(chisq, df, lower.tail = FALSE))
  }
  if(returnValidp)
    res$validp <- p[keep]
  return(res)
}

posNegCountByZscoreFDR <- function(topTbl, fdrCol=NULL, 
                                   thr.logFC.zscore.qnorm=c(0.90, 0.95, 0.99),
                                   thr.fdr=c(0.01, 0.05, 0.10)) {
  if(is.null(fdrCol)) {
    fdrCol <- getFDRCol(colnames(topTbl))
    if(is.na(fdrCol)) {
      stop("FDR column not found")
    }
  }
  
  fdr <- topTbl[, fdrCol]
  logFC <- topTbl$logFC
  logFC.zscore <- scale(logFC)[,1]
  posLogFCthr <- sapply(thr.logFC.zscore.qnorm, function(x) min(logFC[logFC.zscore>=qnorm(x)]))
  negLogFCthr <- sapply(thr.logFC.zscore.qnorm, function(x) max(logFC[logFC.zscore<=(-qnorm(x))]))
  egrid <- expand.grid(thr.logFC.zscore.qnorm, thr.fdr)
  posCounts <- apply(egrid, 1, function(x) sum(logFC.zscore > qnorm(x[1]) & fdr <= x[2]))
  negCounts <- apply(egrid, 1, function(x) sum(logFC.zscore < (-qnorm(x[1])) & fdr <= x[2]))
  df <- data.frame(logFC.zscore.qnorm=rep(egrid[,1],2),
                   FDR=rep(egrid[,2],2),
                   featureCount=nrow(topTbl),
                   type=rep(c("positive", "negative"), each=nrow(egrid)),
                   count=c(posCounts, negCounts))
  logFCthr <- rep(0, nrow(df))
  qnormInd <- match(df$logFC.zscore.qnorm, thr.logFC.zscore.qnorm)
  isPositive <- df$type=="positive"
  logFCthr[isPositive] <- posLogFCthr[qnormInd][isPositive]
  logFCthr[!isPositive] <- negLogFCthr[qnormInd][!isPositive]
  df$logFC <- logFCthr
  
  logFC.zscore.qnorm.label <- sprintf("z(logFC)>=qnorm(%.1f%%)\n(logFC >=%1.2f | <=%1.2f)",
                                      thr.logFC.zscore.qnorm*100,
                                      posLogFCthr, negLogFCthr)
  logFC.zscore.qnorm.label.levels <- logFC.zscore.qnorm.label[order(thr.logFC.zscore.qnorm)]
  qnormLabels <- factor(logFC.zscore.qnorm.label[match(df$logFC.zscore.qnorm,
                                                       thr.logFC.zscore.qnorm)],
                        logFC.zscore.qnorm.label.levels)
  df$logFC.zscore.qnorm.factor <- qnormLabels
  df$FDR.factor <- factor(df$FDR, levels=thr.fdr[order(thr.fdr)])
  
  return(df)
}

getPvalCol <- function(colnames) {
  lcns <- tolower(gsub("[[:punct:]]", "", colnames))
  pvalInd <- grepl("^p", lcns) & !grepl("adj", lcns)
  if(sum(pvalInd)==0)
    return(NA)
  return(colnames[pvalInd])
}
getFDRCol <- function(colnames) {
  lcns <- tolower(gsub("[[:punct:]]", "", colnames))
  fdrInd <- grepl("adj", lcns) | grepl("fdr", lcns)
  if(sum(fdrInd)==0)
    return(NA)
  return(colnames[fdrInd])
}
fishersMethodByList <- function(namedP, list) {
  stopifnot(!is.null(names(list)))
  resList <- lapply(list, function(x) {
    ind <- match(x, names(namedP))
    res <- fishersMethod(namedP[ind], returnValidp=FALSE)
    return(res)
  })
  resDf <- data.frame(geneset=names(list),
                      chisq=sapply(resList, function(x) x$chisq),
                      geneCount=sapply(resList, function(x) x$df/2),
                      p=sapply(resList, function(x) x$p),
                      row.names=NULL)
  resDf$FDR <- p.adjust(resDf$p, "BH")
  return(resDf)
}
getPvec <- function(tbl, pCol, idCol) {
  pvec <- tbl[,pCol]
  names(pvec) <- tbl[,idCol]
  return(pvec)
}
fishersMethodForTopTbl <- function(topTbl, list, 
                                   idCol="ensembl_gene_id", logFCCol="logFC",
                                   pCol=NULL) {
  if(is.null(pCol)) {
    pCol <- getPvalCol(colnames(topTbl))
    if(is.na(pCol)) {
      stop("p-value column not found, please specify it yourselfs")
    }
  }
  
  logFC <- topTbl[,logFCCol]
  
  mixedRes <- fishersMethodByList(getPvec(topTbl, pCol, idCol), list)
  
  posTopTbl <- subset(topTbl, logFC>0)
  posGeneVec <- fishersMethodByList(getPvec(posTopTbl, pCol, idCol), list)
  
  negTopTbl <- subset(topTbl, logFC<0)
  negGeneVec <- fishersMethodByList(getPvec(negTopTbl, pCol, idCol), list)
  
  res1 <- merge(mixedRes, posGeneVec, by="geneset", suffix=c(".mixed", ".positive"))
  colnames(negGeneVec)[2:ncol(negGeneVec)] <- paste(colnames(negGeneVec)[2:ncol(negGeneVec)],
                                                    ".negative", sep="")
  res2 <- merge(res1, negGeneVec, by="geneset")
  return(res2)
}
