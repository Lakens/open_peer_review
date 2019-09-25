#install.packages("pdftools")
#install.packages("stringr")
#install.packages("tm")

library(pdftools)
library(stringr)
library(tm)

#########################list of article id's#####################################
article_id <- vector()

#path <- "royal_society_pdf_files/Article_ID_OB.txt" #used for downloading the pdf's from open biology (list with the open biology ID's)
#path <- "royal_society_pdf_files/Article_ID_OB.txt" #used for downloading the pdf's from open science (list with the open biology ID's)
path <- "royal_society_pdf_files/OB_pdf_list.txt" #uses list in which the non-corrupted pdf id's of open biology are contained
#path <- "royal_society_pdf_files/OS_pdf_list.txt" #uses list in which the non-corrupted pdf id's of open science are contained

conn <- file(path,open="r")
article_id <- readLines(conn)
close(conn)

########################downloading the pdf's#####################################
##this section is used for downloading the pdf files.
# sleep <- function() { Sys.sleep(20) } #needed to prevent an IP block from the royal society website
# 
#  for (i in 1:length(article_id)) {
#    sleep()
#    article_id[i] <- gsub("10.1098/rsob.", "", article_id[i])  #used for open biology
#    #article_id[i] <- gsub("10.1098/rsos.", "", article_id[i]) #used for open science
#    
#    url <- paste("https://royalsocietypublishing.org/action/downloadSupplement?doi=10.1098%2Frsob.", article_id[i], "&file=rsob", article_id[i], "_review_history.pdf", sep="")
#    destfile1 <- paste("C:\\Users\\Nino9000\\Documents\\BEP Open Peer Review\\PDF_Files\\Open Biology\\review", article_id[i], ".pdf", sep="")
#    download.file(url, destfile=destfile1 , mode="wb")
#    }

########################Reading pdf file #########################################
for (i in 1:length(article_id)) {
  pdf_file <- paste("royal_society_pdf_files/open_biology/review", article_id[i], ".pdf", sep="") #map with open biology pdf's
  #pdf_file <- paste("C:\\Users\\Nino9000\\Documents\\BEP Open Peer Review\\PDF_Files\\Open Science\\review", article_id[i], ".pdf", sep="") #map with open science pdf's
  
  if (file.exists(pdf_file)){
  read <- readPDF(control = list(text = "-layout"))
  document <- Corpus(URISource(pdf_file), readerControl = list(reader = read))

############################Text cleaning#########################################
  trs_doc <- content(document[[1]])
  trs_doc <- strsplit(trs_doc, "\r\n")    #turns the list from one item into multiple items based on enters in the pdf's
  trs_doc <- unlist(trs_doc)              #makes sublist items into list items
  trs_doc <- trimws(trs_doc)              #Removes whitespaces at beginning and end of an item
  trs_doc <- gsub("\\s+", " ", trs_doc)   #Removes double enters

##############################Labeling section#####################################
#the labels are used when extracting data from the txt files, they function as marks to keep track what information is stored at which location.
  version_count <- 1
  author_count <- 1
  recommendation_count <- 1
  comment_count <-1
  x <- length(trs_doc)
  y <- 1
  
  while (y<x){
    if (str_sub(trs_doc[y], 1,4) == "RSOB"){  #Search for RSOS or RSOB
      trs_doc <- append(trs_doc, paste("label_version_", version_count, sep="" ), after = y - 1)
      version_count = version_count + 1
      x = x + 1
      y = y + 1
    }
    if (str_sub(trs_doc[y], 1,12) == "Review form:"){
      trs_doc <- append(trs_doc, paste("label_author_", author_count, sep="" ), after = y - 1)
      author_count = author_count + 1
      x = x + 1
      y = y + 1
    }
    if (str_sub(trs_doc[y], 1,15) == "Recommendation"){ #There is a small difference between how the recommendation is coded in Open Science and Open Biology - is is "Recommendation?" in open science and "Recommendation" in Open Biology
      trs_doc <- append(trs_doc, paste("label_recommendation_", recommendation_count, sep="" ), after = y)
      recommendation_count = recommendation_count + 1
      x = x + 1
    }
     if (str_sub(trs_doc[y], 1,22) == "Comments to the Author"){
      trs_doc <- append(trs_doc, paste("label_comment_", comment_count, sep="" ), after = y)
      comment_count = comment_count + 1
       x = x + 1
     }
    if (str_sub(trs_doc[y], 1,15) == "Decision letter"){
      trs_doc <- append(trs_doc, paste("label_end_comment", sep="" ), after = y -1)
      x = x + 1
      y = y + 1
    }
    y = y + 1
  }
  
  trs_doc <- append(trs_doc, "Open Biology") #used for open biology
  #trs_doc <- append(trs_doc, "Society Open") #used for open science

#######################Storing the list into a txt file###########################

  final_txt_file <- paste("royal_society_txt_files/open_biology/review", article_id[i], ".txt", sep="") #location where txt's are stored for Open biology
  #final_txt_file <- paste("C:\\Users\\Nino9000\\Documents\\BEP Open Peer Review\\TXT_Files\\TRS\\Open Science\\Review", article_id[i], ".txt", sep="") #location where txt's are stored for Open science
  
  sink(final_txt_file)
  for (i in 1:length(trs_doc)){
    cat(trs_doc[i])
    cat("\n")
  }
  sink()
  }
}
