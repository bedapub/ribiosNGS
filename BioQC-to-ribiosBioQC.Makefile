## This Makefile is used to convert the public version BioQC into ribiosBioQC
## zhangj83, 17.06.2014

all:clean replace

BioQC:
	git clone git@github.com:Accio/BioQC.git

ribiosBioQC:BioQC
	mv BioQC ribiosBioQC

replace:ribiosBioQC
	rm -rf ribiosBioQC/.git
	find ribiosBioQC -type f -exec sed -i 's/BioQC/ribiosBioQC/g' '{}' \;

clean:
	rm -rf ribiosBioQC

.PHONY=replace