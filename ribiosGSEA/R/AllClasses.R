setClass("gseaResItem",
         representation=list(geneset="character",
           "es"="numeric",
           "nes"="numeric",
           "np"="numeric",
           "fdr"="numeric",
           "fwer"="numeric",
           "geneIndices"="integer",
           "esProfile"="numeric",
           "coreEnrichThr"="numeric"))

setClass("annoGseaResItem",
         representation=list("gsGenes"="character",
           "gsGeneValues"="numeric"),
         contains="gseaResItem")

setClass("annoGseaRes", contains="list")
setClass("annoGseaResList", contains="list") #3 a list of annoGseaRes objects
setClass("GeneSet",
         representation=list(
             category="character",
             name="character",
             desc="character",
             genes="character"),
         prototype=list(category=as.character(NA),
                        name=as.character(NA),
                        desc=as.character(NA),
                        genes=as.character(NA)))
setClass("GeneSets",contain="list")
## setClass("GeneSetsList", contain="list")

setClass("GeneSetResult",
         representation=list(
             gsCategory="character",
             gsName="character",
             gsEffSize="integer",
             p="numeric",
             fdr="numeric"),
         contains="VIRTUAL")
            
## Fisher's exact test
setClass("FisherResult",
         representation=list(hits="character"),
         contains="GeneSetResult")

setClass("FisherResultList",
         representation=list(
             inputName="character",
             input="character",
             universe="character"),
         contain="list")


## Camera result
setClass("CameraResult",
         representation=list(
             correlation="numeric",
             hits="character",
             score="numeric"),
         contains="GeneSetResult")
setClass("CameraResultList",
         representation=list(inputName="character"),
         contain="list")
            
