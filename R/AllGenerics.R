#' @include ribiosNGS.R AllClasses.R

setGeneric("normalize", function(object, ...) standardGeneric("normalize"))

setGeneric("RiboSeq", function(RNA, RPF, groups, ...) standardGeneric("RiboSeq"))

setGeneric("countRNA", function(object) standardGeneric("countRNA"))
setGeneric("countRPF", function(object) standardGeneric("countRPF"))
setGeneric("cpmRNA", function(object,...) standardGeneric("cpmRNA"))
setGeneric("cpmRPF", function(object,...) standardGeneric("cpmRPF"))

setGeneric("dgeList", function(object) standardGeneric("dgeList"))

setGeneric("normFactors", function(object) standardGeneric("normFactors"))

setGeneric("cpmRNAGroupSum", function(object) standardGeneric("cpmRNAGroupSum"))
setGeneric("cpmRPFGroupSum", function(object) standardGeneric("cpmRPFGroupSum"))

setGeneric("cpmFilter", function(object) standardGeneric("cpmFilter"))
setGeneric("convertPDF2PNG", function(object, ...) standardGeneric("convertPDF2PNG"))

setGeneric("setRnks", function(object, names) standardGeneric("setRnks"))

setGeneric("commonBCV", function(x) standardGeneric("commonBCV"))
setGeneric("tagwiseBCV", function(x) standardGeneric("tagwiseBCV"))
setGeneric("trendedBCV", function(x) standardGeneric("trendedBCV"))

setGeneric("BCV", function(x) standardGeneric("BCV"))
setGeneric("plotBCV", function(x, ...) standardGeneric("plotBCV"))

setGeneric("EdgeObject", function(object, designContrast, ...) standardGeneric("EdgeObject"))

setGeneric("volcanoPlot", function(object,...) standardGeneric("volcanoPlot"))
setGeneric("smearPlot", function(object,...) standardGeneric("smearPlot"))

setGeneric("modLogCPM", function(object, ...) standardGeneric("modLogCPM"))

setGeneric("voom", function(object, ...) standardGeneric("voom"))

setGeneric("commonDisp", function(object) standardGeneric("commonDisp"))
setGeneric("commonDisp<-", function(object,value) standardGeneric("commonDisp<-"))
setGeneric("hasCommonDisp", function(object) standardGeneric("hasCommonDisp"))
setGeneric("setCommonDispIfMissing", function(object, common.disp) standardGeneric("setCommonDispIfMissing"))

setGeneric("sniffFeatures", function(object) standardGeneric("sniffFeatures"))

setGeneric("isAnnotated", function(object) standardGeneric("isAnnotated"))


setGeneric("designMatrix<-", function(object, value) standardGeneric("designMatrix<-"))
setGeneric("contrastMatrix<-", function(object, value) standardGeneric("contrastMatrix<-"))

setGeneric("updateDesignMatrixBySVA", 
           function(object, design,  ...) standardGeneric("updateDesignMatrixBySVA"))

setGeneric("inferSV", function(object, design, ...) standardGeneric("inferSV"))
setGeneric("voomSVA", function(object, design, ...) standardGeneric("voomSVA"))
