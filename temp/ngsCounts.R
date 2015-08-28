countToTpm <- function(counts, effLen) {
    
    rate <- log(counts) - log(effLen)
    denom <- log(sum(exp(rate)))
    res <- exp(rate - denom + log(1e6))
    dim(res) <- dim(counts)
    dimnames(res) <- dimnames(counts)
    return(res)
    
}
 
countToFpkm <- function(counts, effLen)
{
    N <- sum(counts)
    exp( log(counts) + log(1e9) - log(effLen) - log(N) )
}
 
fpkmToTpm <- function(fpkm)
{
    exp(log(fpkm) - log(sum(fpkm)) + log(1e6))
}
 
countToEffCounts <- function(counts, len, effLen)
{
    counts * (len / effLen)
}
 
################################################################################
# An example
################################################################################
## cnts <- c(4250, 3300, 200, 1750, 50, 0)
## lens <- c(900, 1020, 2000, 770, 3000, 1777)
## countDf <- data.frame(count = cnts, length = lens)
 
# assume a mean(FLD) = 203.7
## countDf$effLength <- countDf$length - 203.7 + 1
## countDf$tpm <- with(countDf, countToTpm(count, effLength))
## countDf$fpkm <- with(countDf, countToFpkm(count, effLength))
## with(countDf, all.equal(tpm, fpkmToTpm(fpkm)))
## countDf$effCounts <- with(countDf, countToEffCounts(count, length, effLength))
