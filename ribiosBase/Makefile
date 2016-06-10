## AUTOMATICALLY GENERATED FROM TEMPLATE (Fri Sep  4 11:39:55 CEST 2015). DO NOT EDIT IT MANUALLY!
################################################################################
##
##  Makefile
##      Author: Jitao David Zhang <jitao_david.zhang@roche.com>
##	pRED Bioinformatics
##	Pharmaceutical Science, F.Hoffmann-La Roche AG
##      Description: Makefile for building ribios packages
##	
################################################################################

R=R
PKG=$(shell awk 'BEGIN{FS=":"}{if ($$1=="Package") {gsub(/ /, "",$$2);print $$2}}' DESCRIPTION)
PKG_VERSION=$(shell awk 'BEGIN{FS=":"}{if ($$1=="Version") {gsub(/ /, "",$$2);print $$2}}' DESCRIPTION)


PKG_ROOT_DIR=`pwd`
PKG_SRC_DIR=$(PKG_ROOT_DIR)/src

CHECK_FILE=${PKG}_${PKG_VERSION}.tar.gz
CHECK_DIR=${PKG}.Rcheck

roxygenise:
	@echo '====== roxygenize ======'	
	@(cd ..; ${R} --vanilla -q -e "library(roxygen2);roxygenise(\"$(PKG)\")")
	@echo ' '

preinstall:
	@echo '====== Pre-install to get c files compiled ======'
	@(cd ..; ${R} CMD INSTALL ${PKG})
	@echo '====== Installing finished ======'
	@echo ' '

static: preinstall
	@echo '====== Compile the static library  ======'
	@(cd src; ar rcs ../inst/lib/ribiosBase.a *.o)

install: roxygenise static
	@echo '====== Installing Package ======'
	@(cd ..; ${R} CMD INSTALL ${PKG})
	@echo '====== Installing finished ======'
	@echo ' '

check:	dist
	@echo '====== Checking Package ======'
	@(cd ..; ${R} CMD check ${CHECKADD} ${CHECK_FILE})
	@echo '====== Checking finished ======'
	@echo ' '

envcheck: dist
	@echo '====== Checking Package w/o Environmental Vars ======'
	@(cd ..; env -i BIOINFOCONFDIR=${BIOINFOCONFDIR} PATH="/usr/bin/:/usr/local/bin:/bin/:/usr/bin/:/usr/sbin/:/usr/local/bin/:/usr/X11R6/bin:/opt/oracle/client/10/run_1/bin:/usr/kerberos/bin:" LD_LIBRARY_PATH="/homebasel/beda/zhangj83/libs" ${R} CMD check ${CHECKADD} ${PKG}_${PKG_VERSION}.tar.gz) 
	@echo '====== Checking finished ======'

dist:	clean roxygenise
	@echo '====== Building Distribution ======'
	@(cd ..; ${R} CMD build $(PKG))
	@echo '====== Building finished ======'
	@echo ' '

clean:
	@echo '====== Cleaning Package ======'
	@(rm -f $(PKG_SRC_DIR)/*.o $(PKG_SRC_DIR)/*.so)
	@(rm -f ../${CHECK_FILE})
	@(rm -rf ../${CHECK_DIR})
	@(find . -type f -name "*~" -exec rm '{}' \;)
	@(find . -type f -name ".Rhistory" -exec rm '{}' \;)
	@echo ' '
