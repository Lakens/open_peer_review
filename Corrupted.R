library(pdftools)
library(tm)
library(stringr)

article_id <- vector()
article_id_os_open <- vector()
fix_doc <- vector()

path <- "C:\\Users\\Nino9000\\Documents\\BEP Open Peer Review\\TRS_Open_Biology_id.txt"

conn <- file(path,open="r")
article_id <- readLines(conn)
close(conn)

for (r in 1:length(article_id)){
  article_id[r] <- gsub("10.1098/rsob.", "", article_id[r])
  fix_doc[1] <- "nope"
  review_id <- article_id[r]
  document <- NULL

  path <- paste("C:\\Users\\Nino9000\\Documents\\BEP Open Peer Review\\PDF_Files\\Open Biology\\review", review_id, ".pdf", sep="")  #used for open biology
  #path <- paste("C:\\Users\\Nino9000\\Documents\\BEP Open Peer Review\\PDF_Files\\Open Science\\review", review_id, ".pdf", sep="") #used for open science

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

final_txt_file <- "C:\\Users\\Nino9000\\Documents\\BEP Open Peer Review\\OB_pdf_list.txt" #used for open biology
#final_txt_file <- "C:\\Users\\Nino9000\\Documents\\BEP Open Peer Review\\OS_pdf_list.txt" #used for open science

#######################Storing the new list of not corrupted pdf files###########################
sink(final_txt_file)
for (i in 1:length(article_id_os_open)){
  cat(article_id_os_open[i])
  cat("\n")
}
sink()

