PDIR=/SOFT/bi/apps/ribios/ribiosNetwork/inst

## not supported format: gml as fallback
test:
	time ${PDIR}/Rscripts/ronet.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 0 -outfile /tmp/test-ronet.abc

	time ${PDIR}/Rscripts/ronet.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 0 -outfile /tmp/test-ronet.gml
	time ${PDIR}/Rscripts/ronet.Rscript -infile ${PDIR}/extdata/test-ronet-genesymbols.txt -genesymbol -maxConnector 1 -outfile /tmp/test-ronet-genesymbol.gml
	time ${PDIR}/Rscripts/ronet.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 1 -outfile /tmp/test-ronet-max1.gml
	time ${PDIR}/Rscripts/ronet.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 5 -outfile /tmp/test-ronet-max5.gml

	time ${PDIR}/Rscripts/ronet.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 1 -outfile /tmp/test-ronet-max1-ce.gml -connectorEdges

testPublic:
	time ${PDIR}/Rscripts/ronet-public.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 0 -outfile /tmp/test-ronet-public.abc

	time ${PDIR}/Rscripts/ronet-public.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 0 -outfile /tmp/test-ronet-public.gml
	time ${PDIR}/Rscripts/ronet-public.Rscript -infile ${PDIR}/extdata/test-ronet-genesymbols.txt -genesymbol -maxConnector 1 -outfile /tmp/test-ronet-public-genesymbol.gml
	time ${PDIR}/Rscripts/ronet-public.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 1 -outfile /tmp/test-ronet-public-max1.gml
	time ${PDIR}/Rscripts/ronet-public.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 5 -outfile /tmp/test-ronet-public-max5.gml

	time ${PDIR}/Rscripts/ronet-public.Rscript -infile ${PDIR}/extdata/test-ronet-geneids.txt -maxConnector 1 -outfile /tmp/test-ronet-public-max1-ce.gml -connectorEdges
