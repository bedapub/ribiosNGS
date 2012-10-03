#!/bin/bash

PDIR=/SOFT/bi/apps/ribios/ribiosAnnotation/inst/Rscripts
PROG=${PDIR}/ribios_orthologizer.Rscript
INDIR=${PDIR}/../extdata/ribios_orthologizer_testdata/
OUTDIR=${INDIR}

echo "====Examples of ribios_annotate.Rscript===="
echo "==[see help message and usage]=="
${PROG}

echo "==[list all supported chiptypes]=="
${PROG} -chiptype

echo
echo "==[Orthologize GeneID]=="
./ribios_orthologizer.Rscript -infile ${INDIR}/rat-geneid.txt -chiptype GeneID

echo
echo "==[Orthologize GeneSymbol, do NOT remove unmapped ones]=="
./ribios_orthologizer.Rscript -infile ${INDIR}/rat-genesymbol.txt -chiptype genesymbol

echo
echo "==[Orthologize GeneSymbol, remove unmapped ones]=="
./ribios_orthologizer.Rscript -infile ${INDIR}/rat-genesymbol.txt -chiptype genesymbol -delUnmapped

echo
echo "==[Orthologize probesets, given the chiptype]=="
./ribios_orthologizer.Rscript -infile ${INDIR}/rat-probesets.txt -chiptype RAT230_2

echo
echo "==[Orthologize probesets, without chiptype (SLOW)]=="
./ribios_orthologizer.Rscript -infile ${INDIR}/rat-probesets.txt