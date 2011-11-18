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
## conditional: choose R version depending on the BICOSN value
ifneq ($(BICOSN), dev)
	R:= R
else
	R:= /SOFT/bi/apps/R/devel/trunk/bin/R
endif 

PKG          := $(shell awk 'BEGIN{FS=":"}{if ($$1=="Package") {gsub(/ /, "",$$2);print $$2}}' DESCRIPTION)
PKG_VERSION  := $(shell awk 'BEGIN{FS=":"}{if ($$1=="Version") {gsub(/ /, "",$$2);print $$2}}' DESCRIPTION)


PKG_ROOT_DIR := $(shell pwd)
PKG_SRC_DIR := $(PKG_ROOT_DIR)/src

install: 
	@echo '====== Installing Package ======'
	@(cd ..; ${R} CMD INSTALL $(PKG))
	@echo '====== Installing finished ======'
	@echo ' '

check:	dist
	@echo '====== Checking Package ======'
	@(cd ..; ${R} CMD check ${PKG}_${PKG_VERSION}.tar.gz)
	@echo '====== Checking finished ======'
	@echo ' '

dist:	clean
	@echo '====== Building Distribution ======'
	@(cd ..; ${R} CMD build $(PKG))
	@echo '====== Building finished ======'
	@echo ' '

clean:
	@echo '====== Cleaning Package ======'
	@(rm -f $(PKG_SRC_DIR)/*.o $(PKG_SRC_DIR)/*.so)
	@(find . -type f -name "*~" -exec rm '{}' \;)
	@echo ' '
