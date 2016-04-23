setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon_2nd/Melbourne_Datathon_2016/kaggle/noah is a soy sause/")
rm(list = ls()); gc();
require(data.table)
require(tm)
require(stringr)
load("../../../data/RData/dt_jobs.RData")
#######################################################################################
## pdfs ###############################################################################
#######################################################################################
pdf.path <- file.path("..", "..", "..", "data", "glossary", "pdf")
pdf.names <- dir(path.pdf)
pdf.path.full <- file.path(pdf.path, pdf.names)
pdf.path.full
# [1] "../../../data/glossary/pdf/terminology2.pdf"                          
# [2] "../../../data/glossary/pdf/Tourism-Business-Toolkit-VOL1-Chapter7.pdf"
# [3] "../../../data/glossary/pdf/Travel and tourism glossary.pdf"  
######################
## terminology2.pdf ##
######################
## read
if(all(file.exists(Sys.which(c("pdfinfo", "pdftotext"))))) {
    pdf <- readPDF(control = list(text = "-layout"))(elem = list(uri = pdf.path.full[1]),
                                                     language = "en",
                                                     id = "id1")
    content(pdf)[1:13]
}

## convert to txt obj
doc.pdf <- content(pdf)

## position of " - "
pos <- regexpr(" - ", doc.pdf)
glossary.1 <- substr(doc.pdf, 1, pos - 1)[substr(doc.pdf, 1, pos - 1) != ""]

## remove "\f"
glossary.1 <- gsub("\\f", "", glossary.1)

## to lower
glossary.1 <- tolower(glossary.1)

## split
# ()
glossary.1 <- unlist(str_split(glossary.1, " \\("))
glossary.1 <- gsub("\\)", "", glossary.1)
# /
glossary.1 <- unlist(str_split(glossary.1, "/"))

################################################
## Tourism-Business-Toolkit-VOL1-Chapter7.pdf ## # no pattern to crawl, summarised into a txt
################################################
if(all(file.exists(Sys.which(c("pdfinfo", "pdftotext"))))) {
    pdf <- readPDF(control = list(text = "-layout"))(elem = list(uri = pdf.path.full[2]),
                                                     language = "en",
                                                     id = "id1")
    content(pdf)[1:20]
}
## read
glossary.2 <- readLines("../../../data/glossary/txt/glossary_2.txt")

## to lower
glossary.2 <- tolower(glossary.2)

#####################################
## Travel and tourism glossary.pdf ##
#####################################













