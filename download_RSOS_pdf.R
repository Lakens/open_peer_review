#install.packages("pdftools")
#install.packages("stringr")
#install.packages("tm")

library(pdftools)
library(stringr)
library(tm)

#########################list of article id's#####################################
article_id <- vector()

path <- "scopus_export_rsos.csv" #used for downloading the pdf's from open biology (list with the open biology ID's)
#path <- "C:\\Users\\Nino9000\\Documents\\BEP Open Peer Review\\Article_ID_OB.txt" #used for downloading the pdf's from open science (list with the open biology ID's)

conn <- file(path,open="r")
scopus_data <-  read.csv(path, stringsAsFactors = FALSE)
article_id <- scopus_data$DOI
close(conn)

########################downloading the pdf's#####################################
#this section is used for downloading the pdf files.
sleep <- function() { Sys.sleep(20) } #needed to prevent an IP block from the royal society website
i<-1
for (i in 1:length(article_id)) {
  sleep()
  #article_id[i] <- gsub("10.1098/rsob.", "", article_id[i])  #used for open biology
  article_id[i] <- gsub("10.1098/rsos.", "", article_id[i]) #used for open science
  
  #url <- paste("https://royalsocietypublishing.org/action/downloadSupplement?doi=10.1098%2Frsos.", article_id[i], "&file=rsob", article_id[i], "_review_history.pdf", sep="") #for biology
  url <- paste("https://royalsocietypublishing.org/action/downloadSupplement?doi=10.1098%2Frsos.", article_id[i], "&file=rsos", article_id[i], "_review_history.pdf", sep="")
  destfile1 <- paste("royal_society_pdf_files\\open_science_2\\review", article_id[i], ".pdf", sep="")
  download.file(url, destfile=destfile1 , mode="wb")
}
