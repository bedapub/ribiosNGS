## AUTOMATICALLY GENERATED FROM TEMPLATE (Mon Aug 14 11:59:57 CEST 2017). DO NOT EDIT IT MANUALLY!
################################################################################
##
##  Makefile
##      Author: Jitao David Zhang <jitao_david.zhang@roche.com>
##	BEDA TRS, pRED, Hoffmann-La Roche AG
##      Description: Makefile for building distributions etc.
##                   the Makefile provides the following targets:
##                   
##                   - make install  calls R CMD INSTALL
##                   - make check    calls R CMD check (with RUnit)
##                   - make dist     calls R CMD build
##
################################################################################
R:=R

roxygenise:
	@echo '====== roxygenize ======'	
	@(${R} -q -e "library(devtools);document('.')")
	@echo ' '

doVignettes:
	@echo "====== vignettes ======"
	@(${R} -q -e "library(devtools); devtools::build_vignettes()")
	@echo ' '

build:
	@echo '====== Building Distribution ======'
	@(${R} -q -e "library(devtools); devtools::build()")
	@echo '====== Building finished ======'
	@echo ' '

install: 
	@echo '====== Installing Package ======'
	@(${R} -q -e "library(devtools); devtools::install()")
	@echo '====== Installing finished ======'
	@echo ' '

check:
	@echo '====== Checking Package ======'
	@(${R} -q -e "library(devtools);check('.', check_dir=\"..\")")
	@echo '====== Checking finished ======'
	@echo ' '

clean:
	@echo '====== Cleaning Package ======'
	@(rm -f src/*.o src/*.so src/*.dll src/*.rds)
	@(find . -type f -name "*~" -exec rm '{}' \;)
	@(find . -type f -name ".Rhistory" -exec rm '{}' \;)
	@echo ' '
