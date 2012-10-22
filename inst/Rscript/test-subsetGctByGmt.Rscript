#!/bin/bash

if [ -z ${RSCRIPT} ]; then
  RSCRIPT=/SOFT/bi/apps/R/bdeRscript;
fi

if [ -z ${PKGROOT} ]; then
  PKGROOT=/SOFT/bi/apps/ribios/ribiosIO
fi

PDIR=${PKGROOT}/inst/Rscript
PROG=${PDIR}/subsetGctByGmt.Rscript
INDIR=${PDIR}/../extdata/subsetGctByGmt_data/
OUTDIR=${INDIR}

echo "====Examples of ribios_annotate.Rscript===="
echo "==[see help message and usage]=="
${RSCRIPT} ${PROG}

echo "==[list all supported chiptypes]=="
${RSCRIPT} ${PROG} -chiptype

echo
echo "==[Export genes in all GMT genesets]=="
${RSCRIPT} ${PROG} -infile ${INDIR}/test-genesymbol.gct -gmtfile ${INDIR}/test.gmt -outfile ${INDIR}/test-genesymbol-out.gct

echo
echo "==[Export genes in all GMT genesets, in multiple files]=="
${RSCRIPT} ${PROG} -infile ${INDIR}/test-genesymbol.gct -gmtfile ${INDIR}/test.gmt -outfile ${INDIR}/multiout.gct -multifile

echo
echo "==[Export genes in selected GMT genesets, in multiple files]=="
${RSCRIPT} ${PROG} -infile ${INDIR}/test-genesymbol.gct -gmtfile ${INDIR}/test.gmt -setnames HSA-LET-7B,HSA-LET-7A -outfile ${INDIR}/selMultiout.gct -multifile

echo
echo "==[Export genes in selected GMT genesets (case-insensitive), in multiple files]=="
${RSCRIPT} ${PROG} -infile ${INDIR}/test-genesymbol.gct -gmtfile ${INDIR}/test.gmt -setnames HSA-LET-7B,hsa-let-7a-s -outfile ${INDIR}/selMultioutIC.gct -multifile

echo
echo "==[Export annotated genes in selected GMT genesets, in one file]=="
${RSCRIPT} ${PROG} -infile ${INDIR}/test.gct -gmtfile ${INDIR}/test.gmt -chiptype HG_U95A -setnames HSA-LET-7A,HSA-LET-7B -outfile ${INDIR}/selAnnoMultioutIC.gct

echo
echo "==[Export annotated genes in selected GMT genesets (case-insensitive), in multiple files. WITH WARNING]=="
${RSCRIPT} ${PROG} -infile ${INDIR}/test.gct -gmtfile ${INDIR}/test.gmt -chiptype HG_U95A -setnames HSA-LET-7A,HSA-LET-7A-S -outfile ${INDIR}/selAnnoMultioutIC.gct -multifile


echo
echo "==[Clean up test output files]=="
rm -rf ${INDIR}/*_*out*.gct ${INDIR}/test-genesymbol-out.gct ${INDIR}/selAnnoMultioutIC.gct