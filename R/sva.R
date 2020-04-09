#' Apply SVA to transformed count data
#' 
#' 
#' @param counts A matrix of counts
#' @param designMatrix Design matrix
#' @param transformFunc A function to transform the count data
#' @return The SV matrix
#' @examples
#' 
#' set.seed(1887)
#' exCounts <- matrix(rpois(12000, 10), nrow=2000, ncol=6)
#' exCounts[1:100, 2:3] <- exCounts[1:100,2:3]+20
#' exDesign <- model.matrix(~gl(2,3))
#' countsSVA(exCounts, designMatrix=exDesign)
#' 
#' @export countsSVA
countsSVA <- function(counts, designMatrix, 
                      transformFunc=function(counts, designMatrix) 
			      voom(counts, designMatrix)$E,
                      ...) {
  transData <- do.call(transformFunc, list(counts=counts, designMatrix=designMatrix))
  sv <- inferSV(transData, designMatrix, ...)
  return(sv)
}

#' Apply SVA to transformed count data and return the transformed matrix
#' removing the effect of surrogate variables
#' 
#' 
#' @param counts A matrix of counts
#' @param designMatrix Design matrix
#' @param transformFunc A function to transform the count data
#' @return The expression matrix, with SV effects removed
#' @examples
#' 
#' exCounts <- matrix(rpois(12000, 10), nrow=2000, ncol=6)
#' exCounts[1:100, 2:3] <- exCounts[1:100,2:3]+20
#' exDesign <- model.matrix(~gl(2,3))
#' head(countsRemoveSV(exCounts, designMatrix=exDesign))
#' 
#' @importFrom limma removeBatchEffect
#' @export countsRemoveSV
countsRemoveSV <- function(counts, designMatrix,
                           transformFunc=function(counts, designMatrix) 
				   voom(counts, designMatrix)$E) {
  transformedData <- do.call(transformFunc, list(counts=counts, 
						 designMatrix=designMatrix))
  sv <- countsSVA(counts, designMatrix, transformFunc=transformFunc)
  res <- removeBatchEffect(transformedData, covariates=sv, design=designMatrix)
  return(res)
}

#' Apply SVA to voom-transformed count data, and return the voom expression
#' matrix with surrogate variables' effect removed
#' 
#' 
#' @param counts A matrix of counts
#' @param designMatrix Design matrix
#' @return The voom expression matrix, with SV effects removed
#' @examples
#' 
#' exCounts <- matrix(rpois(12000, 10), nrow=2000, ncol=6)
#' exCounts[1:100, 2:3] <- exCounts[1:100,2:3]+20
#' exDesign <- model.matrix(~gl(2,3))
#' head(voomRemoveSV(exCounts, designMatrix=exDesign))
#' ## compare the results without SV removal, note the values in the 
#' ## second and third column are much larger than the rest
#' head(voom(exCounts, exDesign)$E)
#' 
#' @export voomRemoveSV
voomRemoveSV <- function(counts, designMatrix) {
  countsRemoveSV(counts, designMatrix, 
		 transform=function(counts, designMatrix) 
			 voom(counts, designMatrix)$E)
}


#' Apply SVA to cpm-transformed count data
#' 
#' 
#' @param counts A matrix of counts
#' @param designMatrix Design matrix
#' @return The SV matrix
#' @examples
#' 
#' exCounts <- matrix(rpois(12000, 10), nrow=2000, ncol=6)
#' exCounts[1:100, 2:3] <- exCounts[1:100,2:3]+20
#' exDesign <- model.matrix(~gl(2,3))
#' cpmSVA(exCounts, designMatrix=exDesign)
#' 
#' @export cpmSVA
cpmSVA <- function(counts, designMatrix) {
  countsSVA(counts, designMatrix, 
	    transform=function(counts, designMatrix) cpm(counts, log=TRUE))
}

#' Apply cpm to voom-transformed count data, and return the voom expression
#' matrix with surrogate variables' effect removed
#' 
#' 
#' @param counts A matrix of counts
#' @param designMatrix Design matrix
#' @return The cpm matrix, with SV effects removed
#' @importFrom edgeR cpm
#' @export cpm
#' @examples
#' 
#' exCounts <- matrix(rpois(12000, 10), nrow=2000, ncol=6)
#' exCounts[1:100, 2:3] <- exCounts[1:100,2:3]+20
#' exDesign <- model.matrix(~gl(2,3))
#' head(cpmRemoveSV(exCounts, designMatrix=exDesign))
#' ## compare the results without SV removal, note the values in the second 
#' ## and third column are much larger than the rest
#' head(cpm(exCounts))
#' 
#' @export cpmRemoveSV
cpmRemoveSV <- function(counts, designMatrix) {
  countsRemoveSV(counts, designMatrix, 
		 transform=function(counts, designMatrix) cpm(counts, log=TRUE))
}

#' Is the Surrogate Variable (SV) matrix empty
#' 
#' 
#' @param sv A surrogate variable (SV) matrix returned by \code{sva}
#' @return \code{TRUE} if no valid SV was estimated; otherwise \code{FALSE}.
#' @examples
#' 
#' isEmptySV(matrix(0, 1,1))
#' isEmptySV(matrix(rnorm(5), nrow=5))
#' 
#' @export isEmptySV
isEmptySV <- function(sv) {
  nrow(sv)==1 && ncol(sv)==1
}

#' Perform surrogate variable analysis (SVA) to an EdgeObject object
#' 
#' 
#' @param edgeObj An \code{EdgeObject} object
#' @param transform Function name to perform transformation, currently
#' supported values include voom and cpm
#' 
#' The count data associated with the EdgeObject object is first transformed,
#' and surrogate variables are estimated from the transformed data.
#' Correspondingly the design matrix and contrast matrix associated with the
#' object are updated, too.
#' @examples
#' 
#' set.seed(1887)
#' exMat <- matrix(rpois(12000, 10), nrow=2000, ncol=6)
#' exMat[1:100,2:3] <- exMat[1:100, 2:3]+20
#' exGroups <- gl(2,3, labels=c("Group1", "Group2"))
#' exDesign <- model.matrix(~exGroups)
#' exContrast <- matrix(c(-1,1), ncol=1, 
#'               dimnames=list(c("Group1", "Group2"), c("Group2.vs.Group1")))
#' exDescon <- DesignContrast(exDesign, exContrast, groups=exGroups)
#' exFdata <- data.frame(GeneSymbol=sprintf("Gene%d", 1:nrow(exMat)))
#' exPdata <- data.frame(Name=sprintf("Sample%d", 1:ncol(exMat)),
#'                      Group=exGroups)
#' exObj <- EdgeObject(exMat, exDescon, 
#'                      fData=exFdata, pData=exPdata)
#' exSVAobj <- doSVA(exObj, transform="voom")
#' designMatrix(exSVAobj)
#' contrastMatrix(exSVAobj)
#' 
#' ## Note that the SVA is sensitive against parameterisation, see 
#' ## the example below. Also notice that in the zero-intercept parameterisation, 
#' ## the SVA does not give meaningful results.
#' designMatrix(exObj) <- model.matrix(~0+exGroups)
#' designMatrix(doSVA(exObj, transform="voom"))
#' 
#' @export doSVA
doSVA <- function(edgeObj, transform=c("voom", "cpm")) {
  transform <- match.arg(transform)
  counts <- counts(edgeObj)
  design <- designMatrix(edgeObj)
  contrast <- contrastMatrix(edgeObj)
  if(transform=="voom") {
    sv <- voomSVA(counts, design)
  } else if (transform=="cpm") {
    sv <- cpmSVA(counts, design)
  } else {
    stop("Should not get here")
  }
  if(!isEmptySV(sv)) {
    newDesign <- cbind(design, sv)
    designMatrix(edgeObj) <- newDesign
    newContrast <- rbind(contrast, 
                         matrix(0, nrow=ncol(sv), ncol=ncol(contrast),
                                dimnames=list(colnames(sv), 
					      colnames(contrast))))
    contrastMatrix(edgeObj) <- newContrast
  }  
  return(edgeObj)
}
