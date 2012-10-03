#!/bin/bash

if [ -z ${RSCRIPT} ]; then
  RSCRIPT=/SOFT/bi/apps/R/bdeRscript;
fi

if [ -z ${PKGROOT} ]; then
  PKGROOT=/SOFT/bi/apps/ribios/ribiosAnnotation/
fi

PDIR=${PKGROOT}/inst/Rscripts
PROG=${PDIR}/ribios_orthologizer.Rscript
INDIR=${PDIR}/../extdata/ribios_orthologizer_testdata/
OUTDIR=${INDIR}

echo "====Examples of ribios_annotate.Rscript===="
echo "==[see help message and usage]=="
${RSCRIPT} ${PROG}

echo "==[list all supported chiptypes]=="
${RSCRIPT} ${PROG} -chiptype

echo
echo "==[Orthologize GeneID]=="
${RSCRIPT} ./ribios_orthologizer.Rscript -infile ${INDIR}/rat-geneid.txt -chiptype GeneID

echo
echo "==[Orthologize GeneSymbol, do NOT remove unmapped ones]=="
${RSCRIPT} ./ribios_orthologizer.Rscript -infile ${INDIR}/rat-genesymbol.txt -chiptype genesymbol

echo
echo "==[Orthologize GeneSymbol, remove unmapped ones]=="
${RSCRIPT} ./ribios_orthologizer.Rscript -infile ${INDIR}/rat-genesymbol.txt -chiptype genesymbol -delUnmapped

echo
echo "==[Orthologize probesets, given the chiptype]=="
${RSCRIPT} ./ribios_orthologizer.Rscript -infile ${INDIR}/rat-probesets.txt -chiptype RAT230_2

echo
echo "==[Orthologize probesets, without chiptype (SLOW)]=="
${RSCRIPT} ./ribios_orthologizer.Rscript -infile ${INDIR}/rat-probesets.txt