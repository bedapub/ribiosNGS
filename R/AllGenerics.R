setGeneric("normalize", function(object, ...) standardGeneric("normalize"))

setGeneric("RiboSeq", function(RNA, RPF, groups, ...) standardGeneric("RiboSeq"))

setGeneric("countRNA", function(object) standardGeneric("countRNA"))
setGeneric("countRPF", function(object) standardGeneric("countRPF"))
setGeneric("cpmRNA", function(object) standardGeneric("cpmRNA"))
setGeneric("cpmRPF", function(object) standardGeneric("cpmRPF"))

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
