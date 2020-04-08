#' ribiosNGS package provides functionalities used for next-generation sequencing data
#' @docType package
#' @keywords package
NULL


#' @importFrom made4 plotarrays ord
#' @importFrom stringr str_length str_pad
#' @importFrom Biobase `annotation<-` assayData `fData<-` fData `pData<-`
#'             featureNames pData exprs `exprs<-` annotation
#'             featureData phenoData sampleNames
#'             `featureNames<-` storageMode
#' @importMethodsFrom ribiosExpression designMatrix contrastMatrix
#' @importMethodsFrom BiocGenerics counts
#' @export `annotation<-` assayData `fData<-` fData `pData<-`
#' @export featureNames pData exprs `exprs<-` annotation
#' @export featureData phenoData sampleNames
#' @export `featureNames<-`
#' @importFrom ribiosUtils haltifnot assertContrast assertDesign assertDesignContrast ulen assertDir pairwiseJaccardIndex mintersect sortByCol putColsFirst matchColumn pScore basefilename createDir
#' @importFrom ribiosIO read_biokit_exprs writeMatrix gctDesc GctMatrix
#' @importFrom readxl read_excel
#' @importFrom readr count_fields tokenizer_tsv read_tsv
#' @importFrom methods show
#' @importFrom lattice xyplot barchart
#' @importFrom ribiosGSEA biosCamera geneSetPerm gmtListCamera
#' @importFrom ribiosAnnotation querydb annotateGeneIDs annotateGeneSymbols annotatemRNAs
#' @importFrom BiocGenerics counts annotation
#' @importFrom ribiosExpression groups dispGroups designMatrix contrastMatrix contrastNames nContrast contrastSampleIndices truncateDgeTable annotate DesignContrast
#' @importFrom edgeR cpm aveLogCPM estimateGLMRobustDisp rpkm
#' @importFrom graphics pairs
#' @importFrom limma plotMDS zscoreT squeezeVar
#' @importFrom igraph make_empty_graph V graph_from_adjacency_matrix
#' @importFrom sva sva
#' @importFrom gage gage
#' @importFrom BioQC GmtList wmwTest gsName gsDesc gsGenes gsNamespace gsSize matchGenes
#' @importFrom dplyr %>% summarise_all filter
#' @importFrom vsn vsnMatrix meanSdPlot predict
#' @importFrom graphics abline boxplot grid legend panel.smooth par strwidth text
#' @importFrom methods as new slot slotNames validObject
#' @importFrom stats cor fisher.test lm median model.matrix na.omit p.adjust pchisq prcomp quantile t.test update
#' @importFrom utils head read.table str write.table
#' @importFrom ribiosPlot pcaScores
#' @importClassesFrom BioQC GmtList
#' @importClassesFrom Biobase AnnotatedDataFrame ExpressionSet
#' @export dgeWithEdgeR doGse gseWithLogFCgage gseWithCamera sigGene sigGenes sigPosGene sigNegGene sigPosGenes sigNegGenes sigGeneCounts updateSigFilter logFCmatrix model.DGEList countsSVA countsRemoveSV voomSVA voomRemoveSV cpmSVA cpmRemoveSV isEmptySV doSVA cpm.EdgeObject inferSV voomSVA updateDesignMatrixBySVA filterByCPM isAnyNA replaceNAwithZero estimateGLMDisp fitGLM testGLM annotate dgeTables dgeTable writeDgeTables writeTruncatedDgeTables modLogCPM voom logFCgage voomCamera zscoreDGE camera.EdgeResult fullEnrichTable hasCommonDisp setCommonDispIfMissing normBoxplot plotMDS.EdgeObject pairs.EdgeResult volcanoPlot smearPlot isAnnotated readMPS mpsAnnotateReporters doFisher sigGeneBarchart parseMolPhenFeat readMolPhenCoverageGct read_illumina_sampleSheet_xls translationEfficiency doEdgeR doBabel summarizeBabel mergeBabelEdgeR babelAllRnks riboseq.panel xyplot xyplot.RiboSeq riboseq.panel plotRNAMDS plotRPFMDS refBoxplot write.tableList readGseaGeneSet doGeneSetTests cpmBoxplotFile exprsScatterFile mdsFile teBoxplotFile logFCscatterFile babelVolcanoFile rspaceFile upstreamFile functionFile pathwayFile fullTableFile rnkFiles writeHTML readAmpliSeq isCherryPickingRepeat removeCherryPickingRepeat mpsReporterAssocPathway mpsReporterUpstream mpsReporterUpstreamPos mpsReporterUpstreamNeg mpsReporterDownstream mpsReporterAssocGO mpsReporterAssocRCTM readBiokitExpression readBiokitGctFile readBiokitAsDGEList DGEListToLongTable writeDGEList tpm rpkm2tpm edgeRcommand slurmEdgeRcommand slurmEdgeR minGroupCount countByGroup maxCountByGroup hasNoReplicate readCameraResults expandSigCameraResults cameraTable2network visualizeCameraNetworksByContrast parseContributingGenes parseGenesetsContributingGenes parseCameraContributingGenes rowVars prcompExprs prcomp.DGEList checkBiokitSampleAnnotation writeBiokitSampleAnnotation exportEdgeResult staticGeneLevelPlots exportStaticGeneLevelPlots checkContrastNames mergeDGEList vsnSVA
#' @exportClass RiboSeq riboSeqAnalysisObject EdgeResult EdgeObject EdgeGSE DGEList2
#' @exportMethod setRnks RiboSeq normalize countRNA countRPF cpmRNA cpmRPF cpmRNAGroupSum cpmRPFGroupSum cpmFilter featureNames sampleNames commonDisp commonDisp<- BCV show plotBCV dgeList normFactors EdgeObject isAnnotated designMatrix<- contrastMatrix<-
#' @export groups dispGroups designMatrix contrastMatrix contrastNames nContrast DesignContrast
NULL
