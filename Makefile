R=R

full-install: reqR req install

install: install-ribiosUtils install-ribiosArg install-ribiosIO install-ribiosDemo install-ribiosAuth install-ribiosUDIS install-ribiosBioQC install-ribiosPlot install-ribiosAnnotation install-ribiosCGI install-ribiosExpression install-ribiosMath  install-ribiosPCA install-ribiosReposition install-ribiosSeq install-ribiosNetwork install-ribiosQC install-ribiosGSA install-ribiosGSEA install-ribiosNGS 

reqs: reqR reqBioc

reqR:
	${R} -e "install.package(c('rrcov'))"

reqBioc:
	${R} -e "library(BiocInstaller); biocLite(c('Biobase', 'AnnotationDbi', 'limma', 'edgeR','DESeq', 'KEGGgraph', 'e1071', 'gtools', 'gplots', 'RBGL', 'graph', 'igraph', 'lattice'), suppressUpdates=TRUE, suppressAutoUpdate=TRUE)"

install-ribiosUtils:ribiosUtils
	cd ribiosUtils; make install

install-ribiosArg:ribiosArg
	cd ribiosArg; make install	

install-ribiosIO:ribiosIO
	cd ribiosIO; make install

install-ribiosDemo:ribiosDemo
	cd ribiosDemo; make install

install-ribiosAuth:ribiosAuth
	cd ribiosAuth; make install

install-ribiosUDIS:ribiosUDIS
	cd ribiosUDIS; make install

install-ribiosBioQC:ribiosBioQC
	cd ribiosBioQC; make install

install-ribiosPlot:ribiosPlot
	cd ribiosPlot; make install

install-ribiosAnnotation:ribiosAnnotation
	cd ribiosAnnotation; make install

install-ribiosCGI:ribiosCGI
	cd ribiosCGI; make install

install-ribiosExpression:ribiosExpression
	cd ribiosExpression; make install

install-ribiosMath:ribiosMath
	cd ribiosMath; make install

install-ribiosNGS:ribiosNGS
	cd ribiosNGS; make install

install-ribiosNetwork:ribiosNetwork
	cd ribiosNetwork; make install

install-ribiosPCA:ribiosPCA
	cd ribiosPCA; make install

install-ribiosReposition:ribiosReposition
	cd ribiosReposition; make install

install-ribiosSeq:ribiosSeq
	cd ribiosSeq; make install

install-ribiosQC:ribiosQC
	cd ribiosQC; make install

install-ribiosGSA:ribiosGSA
	cd ribiosGSA; make install

install-ribiosGSEA:ribiosGSEA
	cd ribiosGSEA; make install



clean:
	rm *.tar.gz
	find . -maxdepth 1 -type d -name "*.Rcheck" -exec rm -rf '{}' \;

## bioinfolib: run once to convert BIOS to BIOINFOLIB
bioinfolib:
	find . -type f -name Makevars -exec sed -i 's/PRPISRC/BIOSKERNSRC/g' '{}' \;
	find . -type f -name Makevars -exec sed -i 's/PRPIINC/BIOSINC/g' '{}' \;
	find . -type f -name Makevars -exec sed -i 's/prpidefs/biosdefs/g' '{}' \;