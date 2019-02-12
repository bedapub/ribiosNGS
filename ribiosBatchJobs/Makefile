## AUTOMATICALLY GENERATED FROM TEMPLATE (Wed Jan 30 17:38:16 CET 2019). DO NOT EDIT IT MANUALLY!
################################################################################
##
##  Makefile
##      Author: Jitao David Zhang <jitao_david.zhang@roche.com>
##	    F. Hoffmann-La Roche AG
##      Description: Makefile for building distributions etc.
##
################################################################################
R:=R

roxygenise:
	@echo '====== roxygenize ======'	
	@(${R} -q -e "library(devtools);document('.')")
	@echo ' '

test:
	@echo '====== test ======'
	@(${R} -q -e "library(devtools);test('.')")
	@echo 

doVignettes:
	@echo "====== vignettes ======"
	@(${R} -q -e "library(devtools); devtools::build_vignettes()")
	@echo ' '

build: roxygenise doVignettes
	@echo '====== Building Distribution ======'
	@(${R} -q -e "library(devtools); devtools::build()")
	@echo '====== Building finished ======'
	@echo ' '

install: roxygenise doVignettes
	@echo '====== Installing Package ======'
	@(${R} -q -e "library(devtools); devtools::install(upgrade_dependencies=FALSE)")
	@echo '====== Installing finished ======'
	@echo ' '

check: roxygenise
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
