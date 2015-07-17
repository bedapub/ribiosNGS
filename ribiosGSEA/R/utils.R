GSEA_DATA_DIR <- "/data64/bi/httpd_8080/apps/gsea"
GSEA_ANNOTATION_DIR <- file.path(GSEA_DATA_DIR, "annotations")
GSEA_GENESET_DIR <- file.path(GSEA_DATA_DIR, "genesets")
JAVA_BIN <- "/SOFT/bi/apps/java/c/bin/java"
GSEA_JAR <- "/SOFT/bi/apps/gsea/GSEA2-2.02/my_gsea2.jar"
DEFAULT_GMT <- file.path(GSEA_GENESET_DIR,
                         "path.ronet.roche.symbols.gmt")
DEFAULT_CHIP <- file.path(GSEA_ANNOTATION_DIR,
                          "GENE_SYMBOL.chip")

gseaData <- function(file="") {file.path(GSEA_DATA_DIR, file)}
gseaAnno <- function(file="") {file.path(GSEA_ANNOTATION_DIR, file)}
gseaGeneSet <- function(file="") {file.path(GSEA_GENESET_DIR, file)}
dirGseaGeneSet <- function(x) dir(gseaGeneSet())
lsGseaGeneSet <- function(x) dir(gseaGeneSet())

javaBin <- function() {return(JAVA_BIN)}
gseaJar <- function() {return(GSEA_JAR)}
defaultGmt <- function() {return(DEFAULT_GMT)}
defaultChip <- function() {return(DEFAULT_CHIP)}

                          
