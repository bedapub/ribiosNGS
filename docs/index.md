---
layout: default
---
# Installation

You can perform the installation entirely in a [R](https://www.r-project.org) console:

1. Make sure you have the following native development packages installed on your system:
libxml2, libcurl4, libssl, and libssh2
2. Install latest version of [Biocinductor](https://www.bioconductor.org)
```R
## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite()
```
3. Install the following additional Bioconucor packages:
```R
biocLite(c("BioQC", "limma", "graph", "globaltest", "edgeR", "sva", "gage"))
```
4. Install you desired ribios packages, or execute the following lines to install them all at once:
```R
install.packages("devtools")
source("https://raw.githubusercontent.com/Accio/ribios/master/install_ribios_from_github.R")
```

# Packages overview

Package          | description
-----------------|-----------
ribiosBase | RiBIOS (R interface to BIOS) is a collection of R packagesdedicated to bioinformatics, computational biology, and data analysis tasks. Itis built on the thoroughly tested C library BIOS/Bioinfolib that is developedby the Roche Bioinformatics Group. This package provides source code of theBioinfolib and is the foundation of other ribios packages that need to call C-level functions.
 ribiosUtils | ribiosUtils is a swiss-knife package providing misc utilities.
 ribiosIO | Basic data structures and functions for input/output tasks inribios. In this package, only the basic data structures (vectors, lists andmatrices) are allowed, in order to garantee fast loading and highperformance.
 ribiosExpression | Data structures and functions for expression analysis.
 ribiosAuth | Functions for user authentification in Roche bioinformatics system.
 ribiosMath | Mathematical and statistical tools in ribios. Functions target athigh performance.
 ribiosAnnotation | Retrieve annotation information of genomic features, genes, RNAsand proteins from the Oracle database of the Roche Bioinformatics environment
 ribiosCGI | ribiosCGI package implements interface to the CGI module in theBIOS library, which allows R running as CGI backend.
 ribiosUDIS | Access UDIS in ribios via APIs specified in UDIS.
 ribiosGSEA | Gene set enrichment analysis is part of the Roche Bioinformaticspipeline. The ribiosGSEA package provides functions and command-line scripts totrigger an analysis, and to perform integrative analysis of GSEA results.
 ribiosPlot | Data structures, pre-compiled color palettes, functions for datavisualization.
 ribiosArg | Functions to handle command-line arguments in R scripting.
 ribiosNGS | NGS tasks in ribios, primarily differetial gene expressionanalysis.
 ribiosBatchJobs | The package extends the BatchJobs package by allowing submittingjobs to a remote server running Torque. It is used within the RocheBioinformatics Infrastructure.
 ribiosBic | This package provides implementations of biclustering algorithms that are optimized for the BIOS system. Currently two algorithms, Iterative Signature Algorithm (ISA) and QUBIC, are supported. The package provides variants of original algorithms, and functions to interpret biclustering results.
 ribiosDemo | The ribiosDemo package provides prototypes and examplesdemonstrating how to call APIs defined by the BIOS system from R. We hope thispackage assists programmers new to BIOS and/or to R to establish crosstalksbetween two systems. Whoever interested is welcomed to study the source code andmake contributions.
 ribiosDiwa | Differential wiring analysis with microarray profiling data.
 ribiosNetwork | Network analysis toolbox in RIBIOS.
 ribiosPCA | Doing robust principle component analysis with ribios.
 ribiosQC | ribiosQC collects bits and pieces of functions and data structuresfor expression data QC into one package. Its primary use is for Rochebioinformatics microarray/NGS expression pipeline.
 ribiosReposition | Low-level functionalities for computational drug reposition.
 ribiosRiboSeq | Data structure and functions to analyse ribosome sequencing data.
 ribiosROGER | ROche Gene Expression Regulation (ROGER) is an experimentaldatabase storing information of gene expression regulation derived fromdifferential gene expression analysis. ribiosROGER provides functionalities toaccess the database and do operations.
 ribiosSeq | Utilities to analyze sequencing data in the BIOS environment.
