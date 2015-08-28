degreeDensity <- function(histogram) {
    breaks <- histogram$breaks
    midBreaks <- (breaks[-1]+breaks[-length(breaks)])/2
    den <- histogram$density
    df <- data.frame(breaks=midBreaks, density=den)
    return(df)
}
plotDegree <- function(histogram, xlog=TRUE, ylog=TRUE, xlab="Degree", ylab="Density",lmLine=TRUE,...) {
    degDens <- degreeDensity(histogram)
    den <- degDens$density
    midBreaks <- degDens$breaks
    lmModel <- lm(den ~ midBreaks)
    myPanel <- function(x,y,...) {
        panel.xyplot(x,y,...)
        if(lmLine) {
            isOK <- !is.na(x) & !is.na(y) & !is.nan(y) & !is.nan(x) & !is.infinite(y) & !is.infinite(x)
            panel.abline(lm(y[isOK]~x[isOK]), col="red")
        }
    }
    degPlot <- xyplot(den ~ midBreaks, scales=list(x=list(log=xlog), y=list(log=ylog), alternating=1L, tck=c(1,0)),
                      xlab=xlab, ylab=ylab,panel=myPanel,...)
    return(degPlot)
 
}
