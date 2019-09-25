library(pdftools)
library(tm)
library(stringr)
library(here)

here::here()

#set up empty variables
article_id <- vector()
article_id_os_open <- vector()
fix_doc <- vector()

#define path to 
path <- "royal_society_pdf_files/Article_ID_OB.txt"

conn <- file(path,open="r")
article_id <- readLines(conn)
close(conn)

for (r in 1:length(article_id)){
  article_id[r] <- gsub("10.1098/rsob.", "", article_id[r])
  fix_doc[1] <- "nope"
  review_id <- article_id[r]
  document <- NULL

  path <- paste("royal_society_pdf_files\\open_biology\\review", review_id, ".pdf", sep="")  #used for open biology
  #path <- paste("royal_society_pdf_files\open_science\\review", review_id, ".pdf", sep="") #used for open science

  if (file.exists(path)){
    open_file <- file(path,open="r")
    try(read <- readPDF(control = list(text = "-layout")))
    try(document <- Corpus(URISource(path), readerControl = list(reader = read)))
    try(fix_doc <- content(document[[1]])) #this fails when if the file is corrupted resulting in the first item on the fix_doc list remaining "nope"
    
    if (fix_doc[1] != "nope") {
      article_id_os_open <- append(article_id_os_open, article_id[r])
    }
    
    close(open_file)
  }
}

final_txt_file <- "royal_society_pdf_files\OB_pdf_list.txt" #used for open biology
#final_txt_file <- "royal_society_pdf_files\open_biology\OS_pdf_list.txt" #used for open science

#######################Storing the new list of not corrupted pdf files###########################
sink(final_txt_file)
for (i in 1:length(article_id_os_open)){
  cat(article_id_os_open[i])
  cat("\n")
}
sink()

