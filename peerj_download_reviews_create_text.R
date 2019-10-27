# load packages
library(RCurl)
library(XML)
library(stringr)
library(here)

here()

for (r in 1:7930){ #Uses the reviews 1 to 7930 from PeerJ
  
  review_id <- r
  
  # download html
  html <- getURL(paste("https://peerj.com/articles/", review_id, "/reviews/", sep=""), followlocation = TRUE)
  
  #parse html
  PeerJdoc = htmlParse(html, asText=TRUE)
  plain.text <- xpathSApply(PeerJdoc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  
  ############################Text cleaning##########################################
  peerdoc <- paste(plain.text, collapse = "  ")
  peerdoc <- strsplit(peerdoc, "\n\n")
  peerdoc <- unlist(peerdoc)
  peerdoc <- trimws(peerdoc)
  peerdoc <- str_trim(peerdoc)
  peerdoc <- gsub("?", "", peerdoc)
  peerdoc <- gsub("\n", "", peerdoc)
  peerdoc <- gsub("\t", "", peerdoc)
  peerdoc <- gsub("\t\t", "", peerdoc)
  peerdoc <- gsub("\\s+", " ", peerdoc)
  peerdoc <- peerdoc[peerdoc != ""]
  
  ##############################Labeling section#####################################
  #in this section labels get added to the txt files to mark the locations at which important information is contained
  
  version_count <- 1
  author_count <- 1
  x = length(peerdoc)     #for the while loop
  y = 1                   #for the while loop
  
  while (y<x){
    if((str_sub(peerdoc[y], 1, 10) == "Version 0.")){
      peerdoc <- append(peerdoc, paste("label_version_", version_count , sep=""), after= y - 1)
      peerdoc <- append(peerdoc, paste("label_recommendation_", version_count, sep="" ), after=y+4)
      version_count = version_count +1
      x=x+1
      y=y+1
    }
    if(peerdoc[y] == "Basic reporting"){
      peerdoc <- append(peerdoc, paste("label_br_", author_count, sep="" ), after=y)
      peerdoc <- append(peerdoc, paste("label_author_", author_count , sep=""), after= y - 3)
      author_count = author_count + 1
      x=x+1
      y=y+1
    }
    if(peerdoc[y] == "Experimental design"){
      peerdoc <- append(peerdoc, paste("label_ed_", author_count - 1, sep="" ), after=y)
      x=x+1
    }
    if(peerdoc[y] == "Validity of the findings"){
      peerdoc <- append(peerdoc, paste("label_votf_", author_count - 1, sep="" ), after=y)
      x=x+1
    }
    if(peerdoc[y] == "Comments for the author"){
      peerdoc <- append(peerdoc, paste("label_cfta_", author_count - 1, sep="" ), after=y)
      x=x+1
    }
    y=y+1
  }
  
  ############################Adding Section########################################
  #adds the section to which the article belongs to the end of the txt file 
  
  html2 <- getURL(paste("https://peerj.com/articles/", review_id,"/", sep=""), followlocation = TRUE)
  
  PeerJdoc2 = htmlParse(html2)
  plain.text2 <- xpathSApply(PeerJdoc2, "//a/@href")
  
  peerdoc2 <- paste(plain.text2, collapse = "  ")
  peerdoc2 <- strsplit(peerdoc2, " ")
  peerdoc2 <- unlist(peerdoc2)
  mark_count <- 0   #used to skip the first time every section appears
  mark <- 0         #used to store the location where the section appears
  
  for (i in 1:length(peerdoc2)){
    if (peerdoc2[i] == "/sections/aquatic-biology/"){
      mark_count = mark_count + 1
      if (mark_count == 11){
        mark <- i
      }
    }
    if (peerdoc2[i] == "/sections/biochemistry-biophysics-molecular-biology/"){
      mark_count = mark_count + 1
      if (mark_count == 11){
        mark <- i
      }
    }
    if (peerdoc2[i] == "/sections/biodiversity-conservation/"){
      mark_count = mark_count + 1
      if (mark_count == 11){
        mark <- i
      }
    }
    if (peerdoc2[i] == "/sections/bioinformatics-genomics/"){
      mark_count = mark_count + 1
      if (mark_count == 11){
        mark <- i
      }
    }
    if (peerdoc2[i] == "/sections/brain-cognition/"){
      mark_count = mark_count + 1
      if (mark_count == 11){
        mark <- i
      }
    }
    if (peerdoc2[i] == "/sections/ecology/"){
      mark_count = mark_count + 1
      if (mark_count == 11){
        mark <- i
      }
    }
    if (peerdoc2[i] == "sections/environ-sci/"){
      mark_count = mark_count + 1
      if (mark_count == 11){
        mark <- i
      }
    }
    if (peerdoc2[i] == "/sections/microbiology/"){
      mark_count = mark_count + 1
      if (mark_count == 11){
        mark <- i
      }
    }
    if (peerdoc2[i] == "/sections/paleontology-evolutionary-science/"){
      mark_count = mark_count + 1
      if (mark_count == 11){
        mark <- i
      }
    }
    if (peerdoc2[i] == "/sections/plant-biology/"){
      mark_count = mark_count + 1
      if (mark_count == 11){
        mark <- i
      }
    }
    if (peerdoc2[i] == "/sections/zoological-science/"){
      mark_count = mark_count + 1
      if (mark_count == 11){
        mark <- i
      }
    }
  }
  
  section <- peerdoc2[mark]
  section <- gsub("-", " ", section)
  section <- gsub("/sections/", "", section)
  section <- gsub("/", "", section)
  
  if (mark_count == 11){
    peerdoc <- append(peerdoc, section)
  } else {
    peerdoc <- append(peerdoc, "NA")
  }
  
  ##############################Not open acces######################################
  #if the review is not open acces the line "not_open_acces" gets added to the end of the txt file
  
  if (peerdoc[6] == "The author has chosen not to publish the full peer review history of this article."){
    peerdoc <- append(peerdoc, "not_open_acces")
  }
  
  ######################################SINKING#####################################
  #creates the txt file from the list peerdoc
  
  final_txt_file <- paste(review_id, ".txt", sep="")
  
  sink(final_txt_file)
  for (i in 1:length(peerdoc)){
    cat(peerdoc[i])
    cat("\n")
  }
  sink()
}