# required input from environment:
# $(BIOINFOCONFDIR) and $(BICOSN)

DBMS=oracle
include $(BIOINFOCONFDIR)/prpidefs.make
.SUFFIXES:
SHELL=/bin/sh

C=$(BICOSN)/cgi
O=$(BICOSN)/obj
B=$(BICOSN)/bin
H=$(BICOSN)/html
HI=$(BICOSN)/html/images
HIT=$(BICOSN)/html/images/thumb
S=$(BICOSN)/sql

APPINC=      -I$(BICOSN)/conf
APPCONF_H=    $(BICOSN)/conf/appconf.h
APPCONF_MAKE=  $(BICOSN)/conf/appconf.make

CFLAGSL = $(CFLAGS)

# include specific Makefile definitions for this configuration,
# if any:
sinclude $(BICOSN)/conf/appconf.make

# ----------------------------- entry points ------------

compile: compileNoMsg
	@echo "BICOSN=$(BICOSN): compile ok, sql generated. use 'make install' to release."

install: compileNoMsg
	cp-p $O/ribios_cgi $C
	mkdir -p ${HI}
	mkdir -p ${HIT}
	cp-p images/ribios-webBanner.png ${HI}
	cp-p ribios_cgi.js ribios_cgi.css $H
	cp-p NEWS $H
	@echo "BICOSN=$(BICOSN): release ok."

uninstall:
	/bin/rm -f $C/ribios_cgi
	/bin/rm -f ${HIT}/*
	/bin/rmdir ${HIT}
	/bin/rm -f ${HI}/*
	/bin/rmdir ${HI}
	/bin/rm -f $H/*

clean:
	/bin/rm -rf *~ $O/* $(BICOSN)/conf/* $S/* $B/* $H/*  ${CHIPTYPES_FILE}

obj: $O/ribios_cgi

compileNoMsg: $(APPCONF_MAKE) $(APPCONF_H) obj

# usage notes:
# 'make clean' is provided for SUN for forcing a full-remake
# 'make -u obj' is useful on SGI to force making all objects
#               whitout using 'make clean'

# ----------------------------- make things ------------

$(APPCONF_H): appconf.h.src
	evalif C BICOSN_$(BICOSN) appconf.h.src $(APPCONF_H)

$(APPCONF_MAKE): appconf.make.src
	evalif make BICOSN_$(BICOSN) appconf.make.src $(APPCONF_MAKE)
	@echo "BICOSN=$(BICOSN): $(APPCONF_MAKE) generated. Ignore Error 99 and RERUN make!"
	exit 99


# ----------------------------- C programs  ------------
##$O/bios_hm.o: bios_hm.c $(PRPILIB) $(PRPIDBMSLIB) $(APPCONF_H)
##	cc $(CFLAGS) $(APPINC) $(PRPIINC) bios_hm.c  -c -o $O/bios_hm.o 

##$O/bios_hm: $O/bios_hm.o
##	@/bin/rm -f $O/bios_hm
##	cc $(CFLAGSL) $O/bios_hm.o -o $O/bios_hm $(PRPIDBMSLNK) $(PRPILNK)


# ----------------------------- CGI  ------------ 
$O/ribios_cgi.o: ribios_cgi.c $(PRPILIB)  $(APPCONF_H)
	cc $(CFLAGS) $(APPINC) $(PRPIINC) ribios_cgi.c  -c -o $O/ribios_cgi.o 

$O/ribios_cgi: $O/ribios_cgi.o
	@/bin/rm -f $O/ribios_cgi
	cc $(CFLAGSL) $O/ribios_cgi.o -o $O/ribios_cgi  $(PRPILNK) 

