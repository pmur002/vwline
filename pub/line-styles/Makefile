
TARFILE = ../line-styles-deposit-$(shell date +'%Y-%m-%d').tar.gz

%.xml: %.cml
	# Protect HTML special chars in R code chunks
	Rscript -e 't <- readLines("$*.cml"); writeLines(gsub("str>", "strong>", gsub("<rcode([^>]*)>", "<rcode\\1><![CDATA[", gsub("</rcode>", "]]></rcode>", t))), "$*.xml")'
	Rscript toc.R $*.xml
	Rscript bib.R $*.xml

%.Rhtml : %.xml
	# Transform to .Rhtml
	xsltproc knitr.xsl $*.xml > $*.Rhtml

%.html : %.Rhtml
	# Use knitr to produce HTML
	Rscript -e 'library(knitr); knit("$<")'

docker:
	sudo docker run -v $(shell pwd):/home/work/ -w /home/work --rm pmur002/line-styles make line-styles.html

web:
	make docker
	cp -r ../line-styles ~/Web/Reports/VWline/

zip:
	make docker
	tar zcvf $(TARFILE) ./*
