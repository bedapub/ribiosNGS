PDIR=/SOFT/bi/apps/ribios/ribiosNetwork/inst

.PHONY=test testdb testEvaluation testdbEvaluation

## not supported format: gml as fallback
test: testdb
	time ${PDIR}/Rscripts/ronet.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 0 -outfile /tmp/test-ronet.abc

	time ${PDIR}/Rscripts/ronet.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 0 -outfile /tmp/test-ronet.gml
	time ${PDIR}/Rscripts/ronet.Rscript -infile ${PDIR}/extdata/test-ronet-genesymbols.txt -genesymbol -maxConnector 1 -outfile /tmp/test-ronet-genesymbol.gml
	time ${PDIR}/Rscripts/ronet.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 1 -outfile /tmp/test-ronet-max1.gml
	time ${PDIR}/Rscripts/ronet.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 5 -outfile /tmp/test-ronet-max5.gml

	time ${PDIR}/Rscripts/ronet.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 1 -outfile /tmp/test-ronet-max1-ce.gml -connectorEdges

testdb:
	time ${PDIR}/Rscripts/ronet.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 0 -outfile /tmp/test-ronet-max1-ce.gml -db 000001
	time ${PDIR}/Rscripts/ronet.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 0 -outfile /tmp/test-ronet-max1-ce.gml -db 100001

testEvaluation:testdbEvaluation
	time ${PDIR}/Rscripts/ronet-evaluation.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 0 -outfile /tmp/test-ronet-evaluation.abc

	time ${PDIR}/Rscripts/ronet-evaluation.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 0 -outfile /tmp/test-ronet-evaluation.gml
	time ${PDIR}/Rscripts/ronet-evaluation.Rscript -infile ${PDIR}/extdata/test-ronet-genesymbols.txt -genesymbol -maxConnector 1 -outfile /tmp/test-ronet-evaluation-genesymbol.gml
	time ${PDIR}/Rscripts/ronet-evaluation.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 1 -outfile /tmp/test-ronet-evaluation-max1.gml
	time ${PDIR}/Rscripts/ronet-evaluation.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 5 -outfile /tmp/test-ronet-evaluation-max5.gml

	time ${PDIR}/Rscripts/ronet-evaluation.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 1 -outfile /tmp/test-ronet-evaluation-max1-ce.gml -connectorEdges

testdbEvaluation:
	time ${PDIR}/Rscripts/ronet-evaluation.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 0 -outfile /tmp/test-ronet-max1-ce.gml -db 0000011
	time ${PDIR}/Rscripts/ronet-evaluation.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 0 -outfile /tmp/test-ronet-max1-ce.gml -db 1000011
