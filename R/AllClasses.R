setClass("gseaResItem",
         slots=c(geneset="character",
           "es"="numeric",
           "nes"="numeric",
           "np"="numeric",
           "fdr"="numeric",
           "fwer"="numeric",
           "geneIndices"="integer",
           "esProfile"="numeric",
           "coreEnrichThr"="numeric"))

setClass("annoGseaResItem",
         slots=c("gsGenes"="character",
           "gsGeneValues"="numeric"),
         contains="gseaResItem")

setClass("annoGseaRes", contains="list")
setClass("annoGseaResList", contains="list") #3 a list of annoGseaRes objects
setClass("GeneSets", contain="list")

