library(pdftools)
library(tm)
library(stringr)
library(here)

here::here()

#########################list of article id's#####################################
article_id <- vector()
# Create a list of all PDF files in the folder
article_id <- str_extract(list.files(path = "royal_society_pdf_files\\open_science\\", pattern = "\\.pdf$"), "[0-9]+") #list all files in the folder and extract article ID

for (r in 1:length(article_id)){
  fix_doc[1] <- "corrupted"
  review_id <- article_id[r]
  document <- NULL

  path <- paste("royal_society_pdf_files\\open_science\\review", review_id, ".pdf", sep="") #used for open science

  if (file.exists(path)){
    open_file <- file(path,open="r")
    try(read <- readPDF(control = list(text = "-layout")))
    try(document <- Corpus(URISource(path), readerControl = list(reader = read)))
    try(fix_doc <- content(document[[1]])) #this fails when if the file is corrupted resulting in the first item on the fix_doc list remaining "corrupted"
    
    if (fix_doc[1] != "corrupted") {
      article_id_os_open <- append(article_id_os_open, article_id[r])
    }
    close(open_file)
  }
}

final_txt_file <- "royal_society_pdf_files\\OS_pdf_list.txt" #used for open science

#######################Storing the new list of not corrupted pdf files###########################
sink(final_txt_file)
for (i in 1:length(article_id_os_open)){
  cat(article_id_os_open[i])
  cat("\n")
}
sink()
