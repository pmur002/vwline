
## Execute as Rscript toc.R <filename>
args <- commandArgs()
extras <- grep("--args", args) + 1
filename <- args[extras[1]]

## Generate HTML from .bib
filestub <- gsub(".xml", "", filename)
bibfile <- paste0(filestub, ".bib")
bibstub <- paste0(filestub, "-bib")
bibhtml <- paste0(bibstub, ".html")
bibhtmlbib <- paste0(bibstub, "_bib.html")
system(paste0("bibtex2html -a -dl -s apalike -o ", bibstub, " ", bibfile))

## Include bibhtml into reference seection
library(xml2)
xml <- read_xml(filename)
bib <- read_html(bibhtml)

reflist <- xml_find_first(xml,
                          "//ul[preceding-sibling::h2[contains(., 'References')]]")
biblist <- xml_find_first(bib, "//dl")
items <- xml_find_all(reflist,
                      "li/a/@name")
keys <- xml_text(items)
for (i in seq_along(items)) {
    ## Reference label <dt>
    bibdt <- xml_find_first(bib, paste0("//dt[a/@name = '", keys[i], "']"))
    ## Reference label text
    bibref <- xml_text(bibdt)
    ## Replace text of <a> in text with reference label text
    docref <- xml_find_all(xml, paste0("//a[@href = '#", keys[i], "']"))
    for (j in seq_along(docref)) {
        xml_text(docref[j]) <- gsub("\n|[[]|[]]", "", bibref)
    }
}
## Replace <ul> in XML with <dl> from bib
xml_replace(reflist, biblist)

## write out modified XML
write_xml(xml, filename)
