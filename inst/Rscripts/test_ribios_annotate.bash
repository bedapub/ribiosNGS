#!/bin/bash

PDIR=/SOFT/bi/apps/ribios/ribiosAnnotation/inst/Rscripts
PROG=${PDIR}/ribios_annotate.Rscript
INDIR=${PDIR}/ribios_annotate_testdata
OUTDIR=${INDIR}

echo "====Examples of ribios_annotate.Rscript===="
echo "==[see help message and usage]=="
${PROG}

echo "==[list all supported chiptypes]=="
${PROG} -allChiptypes

echo
echo "==[annotate single file (HG-U133_PLUS_2)]=="
${PROG} -infile ${INDIR}/hgu133plus2_probes.txt -chiptype HG-U133_PLUS_2 -outfile ${OUTDIR}/hgu1_ann.txt

echo "==[annotate multiple files (HG-U133_PLUS_2)]=="
${PROG} -infile ${INDIR}/hgu133plus2_probes.txt ${INDIR}/hgu133plus2_probes2.txt -chiptype HG-U133_PLUS_2 -outfile ${OUTDIR}/hgu1_ann.txt ${OUTDIR}/hgu2_ann.txt

echo "==[annotate GeneIDs]=="
${PROG} -infile ${INDIR}/geneids.txt -chiptype GeneID -outfile ${OUTDIR}/geneid_ann.txt

echo "==[annotate GeneSymbols]=="
${PROG} -infile ${INDIR}/genesymbols.txt -chiptype GeneID -outfile ${OUTDIR}/genesymbol_ann.txt

echo "====End of examples===="
