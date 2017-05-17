
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

items <- xml_find_all(xml,
                      "//ul[preceding-sibling::h2[contains(., 'References')]]/li/a/@name")
keys <- xml_text(items)
for (i in seq_along(items)) {
    bibdt <- xml_find_first(bib, paste0("//dt[a/@name = '", keys[i], "']"))
    bibdl <- xml_find_first(bib, paste0("//dt[a/@name = '", keys[i], "']/following-sibling::dd"))
    xml_add_sibling(xml_parent(items[i]), bibdl)
    xml_add_sibling(xml_parent(items[i]), bibdt)
}

## Replace <toc/> element with this content
write_xml(xml, filename)
