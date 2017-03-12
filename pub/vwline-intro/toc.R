
## Execute as Rscript toc.R <filename>
args <- commandArgs()
extras <- grep("--args", args) + 1
filename <- args[extras[1]]

## Generate table of contents with hyperlinks
library(xml2)
xml <- read_xml(filename)
anchors <- xml_find_all(xml, "//h2/a")
names <- xml_attr(anchors, "name")
headings <- xml_text(anchors)
toc <- paste("<div><h2>Table of Contents:</h2><ul>",
             paste0('<li><a href="#', names, '">', headings, '</a></li>',
                    collapse=""),
             "</ul></div>", collapse="")

## Replace <toc/> element with this content
xml_replace(xml_find_first(xml, "//toc"), read_xml(toc))
write_xml(xml, filename)
