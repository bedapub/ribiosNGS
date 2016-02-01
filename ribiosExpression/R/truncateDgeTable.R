#' Transform limma::topTable results to a DGEtable
#' @param limmaTopTable: topTable returned by limma::topTable
#' @return A data.frame known as DGEtable which has controlled column names
#' @examples
#' library(limma)
#' example.sd <- 0.3*sqrt(4/rchisq(100,df=4))
#' example.y <- matrix(rnorm(100*6,sd=example.sd),100,6)
#' example.y[1:2,4:6] <- example.y[1:2,4:6] + 2
#' rownames(example.y) <- paste("Gene",1:100)
#' example.design <- cbind(Grp1=1,Grp2vs1=c(0,0,0,1,1,1))
#' example.fit <- lmFit(example.y,example.design)
#' example.fit <- eBayes(example.fit)
#' example.tt <- topTable(example.fit, coef=2)
#' example.dt <- limmaTopTable2dgeTable(example.tt)
#' head(example.dt)
limmaTopTable2dgeTable <- function(limmaTopTable) {
    colnames(limmaTopTable)[colnames(limmaTopTable)=="P.Value"] <- "PValue"
    colnames(limmaTopTable)[grepl("adj\\.P\\.", colnames(limmaTopTable))] <- "FDR"
    limmaTopTable$Feature <- rownames(limmaTopTable)
    limmaTopTable <- putColsFirst(limmaTopTable,
                                  c("Feature", "AveExpr", "t", "logFC", "PValue", "FDR", "B"))
    return(limmaTopTable)
}

#' Truncate dgeTable into tables of positively and negatively differentially expressed genes according to the pre-defined criteria
#' @param dgeTable dgeTable A DGEtable defined in ribiosExpression. Notice that the column names returned by limma::topTable are remapped (see limmaTopTable2dgeTable).
#' @return A list of two elements: 'pos' and 'neg'. Each contains a dgeTable of positively/negatively regulated genes
#' @references The logic is described at http://rochewiki.roche.com/confluence/display/BIOINFO/Substream+Algorithm

truncateDgeTable <- function(dgeTable) {
    dgeTable <- sortByCol(dgeTable, "PValue", decreasing=FALSE)
    cond1 <- with(dgeTable, abs(logFC)>=1 & FDR<0.10)
    cond2 <- with(dgeTable, abs(logFC)>=1 & PValue<0.05)
    if(sum(cond1)>=200) {
        posTbl <- subset(dgeTable[cond1,], logFC>0)
        negTbl <- subset(dgeTable[cond1,], logFC<0)
    } else if(sum(cond2)>=200) {
        posTbl <- subset(dgeTable[cond2,], logFC>0)
        negTbl <- subset(dgeTable[cond2,], logFC<0)
    } else {
        ntop <- pmin(400,
                     pmin(nrow(dgeTable),
                          pmax(100, as.integer(nrow(dgeTable)*0.05))))
        posTbl <- subset(dgeTable[1:ntop,], logFC>0)
        negTbl <- subset(dgeTable[1:ntop,], logFC<0)
    }
    maxRow <- 150
    if(nrow(posTbl)>maxRow) posTbl <- posTbl[1:maxRow,]
    if(nrow(negTbl)>maxRow) negTbl <- negTbl[1:maxRow,]
    return(list(pos=posTbl, neg=negTbl))
}
