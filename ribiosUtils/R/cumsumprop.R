#' Proportion of cumulative sum over sum
#' 
#' @param x Numeric vector
#' @return the proportion cumulative sum over sum
#' 
#' @examples
#' x <- 1:4
#' cumsumprop(x) ## 0.1, 0.3, 0.6, 1
cumsumprop <- function(x) cumsum(x)/sum(x)

#' Sort a numeric vector and filter by a threshold of cumsumprop
#' @param x Numeric vector, usually named
#' @param thr Threshold, default 0.9, meaning that items whose proportion of cumulative sum just above 0.9 are kept.
#' @return Another numeric vector, likely shorter than x, items whose \code{cumsumprop} is equal or lower than \code{thr}. The rest items are summed into one new item, with the name \code{rest}
#' 
#' This function can be useful to extract from a long numeric vector the largest items that dominate the sum of the vector
#' 
#' @examples
#' x <- c("A"=1,"B"=2,"C"=3,"D"=4,"E"=400,"F"=500)
#' sortAndFilterByCumsumprop(x, thr=0.99) ## F and E should be returned
sortAndFilterByCumsumprop <- function(x, thr=.9) {
  xs <- sort(x, decreasing=TRUE)
  xprop <- cumsumprop(xs)
  firstOver <- which(xprop >= thr)[1]
  isSel <- seq(along=xprop) <= firstOver
  res <- c(xs[isSel], rest=sum(xs[!isSel]))
  return(res)
}


#' Merge infrequent levels by setting the threshold of the proportion of cumulative sum over sum a.k.a. cumsumprop
#' @param classes Character strings or factor.
#' @param thr Numeric, between 0 and 1, how to define frequent levels. Default: 0.9, namely levels which make up over 90\% of all instances.
#' @param mergedLevel Character, how the merged level should be named. Default: \code{rest}.
#' @param returnFactor Logical, whether the value returned should be coereced into a factor.
#' 
#' @note
#' In case only one class is deemed as infrequent, its label is unchanged.
#' 
#' @return A character string vector or a factor, of the same length as the input \code{classes}, but with potentially fewer levels.
#' 
#' @examples 
#' set.seed(1887)
#' myVals <- sample(c(rep("A", 4), rep("B", 3), rep("C", 2), "D"))
#' mergeInfreqLevelsByCumsumprop(myVals, 0.9) ## since A, B, C make up of 90% of the case, D is infrequent. Since it is alone, it is not merged
#' mergeInfreqLevelsByCumsumprop(myVals, 0.9, returnFactor=FALSE) ## return characters
#' mergeInfreqLevelsByCumsumprop(myVals, 0.8) ## since A and B make up 70% of the case, and A, B, C 90%, they are all frequent and D is infrequent. Following the logic above, no merging happens
#' mergeInfreqLevelsByCumsumprop(myVals, 0.7) ## A and B are left, C and D are merged
#' mergeInfreqLevelsByCumsumprop(myVals, 0.5) ## A and B are left, C and D are merged
#' mergeInfreqLevelsByCumsumprop(myVals, 0.4) ## A is left
#' mergeInfreqLevelsByCumsumprop(myVals, 0.3) ## A is left
mergeInfreqLevelsByCumsumprop <- function(classes, thr=.9, mergedLevel="rest", returnFactor=TRUE) {
  x <- table(classes)
  xs <- sort(x, decreasing=TRUE)
  xprop <- cumsumprop(xs)
  firstOver <- which(xprop >= thr)[1]
  isSel <- seq(along=xprop) <= firstOver
  if(sum(!isSel)!=1) {
    minorClasses <- names(xs[!isSel])
    classes[classes %in% minorClasses] <- mergedLevel
    if(returnFactor)
      classes <- factor(classes, levels=c(names(xs[isSel]), mergedLevel))
  } else {
    if(returnFactor)
      classes <- factor(classes, levels=names(xs))
  }
  return(classes)
}
