###Notes###
#install.packages("tidytext")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("textdata") #for sentiment analysis

library(dplyr)
library(tidytext)
library(pdftools)
library(tm)
library(stringr)
library(here)

setwd("C:/Users/Daniel/surfdrive/R/open_peer_review/peerj_reviews_txt")
here()
#########################################Getting the lexicons######################################################
nrc_negative <- vector()
nrc_positive <- vector()

nrc <- get_sentiments("nrc")

for (i in 1:length(nrc$sentiment)){
  if (nrc$sentiment[i] == "negative")
    nrc_negative <- append(nrc_negative, nrc$word[i])
}
for (i in 1:length(nrc$sentiment)){
  if (nrc$sentiment[i] == "positive")
    nrc_positive <- append(nrc_positive, nrc$word[i])
}

#########################################variables used to create the dataframe#################################
df_link <- c()
df_section <- c()
df_days <- c()
df_version <- c()
df_recommendation <- c()
df_word_count <- c()
df_masked <- c()
df_positive <- c()
df_negative <- c()
df_id <- c()
open_reviews <- 0
closed_reviews <- 0
total_version <- 0


#########################################read txt file##########################################################
for (r in 1:1){ #starting the loop to create the dataframe from the 6800 PeerJ articles
  
  review_id <- r
  
  path <- paste(review_id,".txt", sep="")
  
  open_file <- file(path, open = "r")
  lines <- readLines(open_file)
  lines <- trimws(lines)
  close(open_file)
  
  a <- length(lines)
  if (lines[a] != "not_open_acces"){ #If the last line of the file is "not_open_acces" it means: The author has chosen not to publish the full peer review history of this article.
    open_reviews = open_reviews + 1 #If reviews are open, add 1 to counter.
    
    ######variables######
    
    ###############################################Link############################################################
    
    link <- "" #reset link
    link <- paste("https://peerj.com/articles/", review_id, "/reviews/", sep="") 
    
    ###############################Word count, positive and negative###############################################
    
    word_count <- vector()  #Vector that contains the wordcounts
    positive <- vector()    #Vector that contains the positive word count
    negative <- vector()    #Vector that contains the negative word count
    review_count <- 1        #Counts which review is being counted
    x <- 0                  #counter for word_count position
    
    # PeerJ subdivides the review in 4 sections: 
    # Basic reporting (under label_br), experimental design (label_ed), 
    # validity of findings (label_votf), and comments for the authors (label_cfta)
    # We add the words for all these sections under word count.
    # 
    for (i in 1:length(lines)){
      if(lines[i] == paste("label_br_", review_count, sep="")){
        x = x + 1
        word_count[x] <- 0
        positive[x] <- 0
        negative[x] <- 0
        word_count[x] <- word_count[x] + sapply(gregexpr("[[:alnum:]]+", lines[i+1]), function(x) sum(x > 0))
        
        # sep_word <- strsplit(lines[i+1], " ")
        # sep_word <- unlist(sep_word)
        # d <- data.frame(sep_word, stringsAsFactors = FALSE)
        # 
        # for (j in 1:length(d$sep_word)){
        #   d$sep_word[j] <- gsub(",", "", d$sep_word[j])
        #   d$sep_word[j] <- gsub("\\.", "", d$sep_word[j])
        #   for (k in 1:length(nrc_negative)){
        #     if (d$sep_word[j] == nrc_negative[k]){
        #       negative[x] = negative[x] + 1
        #     }
        #   }
        #   for (l in 1:length(nrc_positive)){
        #     if (d$sep_word[j] == nrc_positive[l]){
        #       positive[x] = positive[x] + 1
        #     }
        #   }
        # }
      }
      
      if(lines[i] == paste("label_ed_", review_count, sep="")){
        word_count[x] <- word_count[x] + sapply(gregexpr("[[:alnum:]]+", lines[i+1]), function(x) sum(x > 0))
        
        # sep_word <- strsplit(lines[i+1], " ")
        # sep_word <- unlist(sep_word)
        # d <- data.frame(sep_word, stringsAsFactors = FALSE)
        # 
        # for (j in 1:length(d$sep_word)){
        #   d$sep_word[j] <- gsub(",", "", d$sep_word[j])
        #   d$sep_word[j] <- gsub("\\.", "", d$sep_word[j])
        #   for (k in 1:length(nrc_negative)){
        #     if (d$sep_word[j] == nrc_negative[k]){
        #       negative[x] = negative[x] + 1
        #     }
        #   }
        #   for (l in 1:length(nrc_positive)){
        #     if (d$sep_word[j] == nrc_positive[l]){
        #       positive[x] = positive[x] + 1
        #     }
        #   }
        # }
      }
      
      if(lines[i] == paste("label_votf_", review_count, sep="")){
        word_count[x] <- word_count[x] + sapply(gregexpr("[[:alnum:]]+", lines[i+1]), function(x) sum(x > 0))
        
        # sep_word <- strsplit(lines[i+1], " ")
        # sep_word <- unlist(sep_word)
        # d <- data.frame(sep_word, stringsAsFactors = FALSE)
        # 
        # for (j in 1:length(d$sep_word)){
        #   d$sep_word[j] <- gsub(",", "", d$sep_word[j])
        #   d$sep_word[j] <- gsub("\\.", "", d$sep_word[j])
        #   for (k in 1:length(nrc_negative)){
        #     if (d$sep_word[j] == nrc_negative[k]){
        #       negative[x] = negative[x] + 1
        #     }
        #   }
        #   for (l in 1:length(nrc_positive)){
        #     if (d$sep_word[j] == nrc_positive[l]){
        #       positive[x] = positive[x] + 1
        #     }
        #   }
        # }
      }
      
      if(lines[i] == paste("label_cfta_", review_count, sep="")){
        word_count[x] <- word_count[x] + sapply(gregexpr("[[:alnum:]]+", lines[i+1]), function(x) sum(x > 0))
        
        # sep_word <- strsplit(lines[i+1], " ")
        # sep_word <- unlist(sep_word)
        # d <- data.frame(sep_word, stringsAsFactors = FALSE)
        # 
        # for (j in 1:length(d$sep_word)){
        #   d$sep_word[j] <- gsub(",", "", d$sep_word[j])
        #   d$sep_word[j] <- gsub("\\.", "", d$sep_word[j])
        #   for (k in 1:length(nrc_negative)){
        #     if (d$sep_word[j] == nrc_negative[k]){
        #       negative[x] = negative[x] + 1
        #     }
        #   }
        #   for (l in 1:length(nrc_positive)){
        #     if (d$sep_word[j] == nrc_positive[l]){
        #       positive[x] = positive[x] + 1
        #     }
        #   }
        
        review_count = review_count + 1
      }
    }
    
    ###################################Recommendation, Version and Masked##########################################
    rec <- NA
    recommendation <- vector()
    timelapse <- vector()
    recommendation_count <- 1
    masked <- vector()
    version <- vector()
    adj_recommendation <- vector()
    author_count <- 1
    version_count <- 1
    
    
    for (i in 1:length(lines)){
      if(lines[i] == paste("label_recommendation_", recommendation_count, sep="")){
        if (lines[i+3] == "Accept"){
          rec <- 1 # 1 = accept
        }
        if (lines[i+3] == "Minor Revisions"){
          rec <- 2 # 2 = minor revisions
        }
        if (lines[i+3] == "Major Revisions"){
          rec <- 3 #3 = major revisions
        }
        if (lines[i+3] == "Reject"){
          rec <- 4 #reject
        }
        if (recommendation_count == 1){
          timelapse <- append(timelapse, lines[i-1])
        }
        recommendation_count = recommendation_count +1
      }
      
      
      if (lines[i] == paste("label_version_", version_count, sep="")){
        version_count <- version_count +1
        total_version <- total_version +1
      }
      if (lines[i] == paste("label_author_", author_count, sep="")){
        if (grepl("Reviewer", lines[i+1])){
          masked <- append(masked, 1)
          version <- append(version, version_count - 1)
          recommendation <- append(recommendation, rec)
        } else {
          masked <- append(masked, 0)
          version <- append(version, version_count - 1)
          recommendation <- append(recommendation, rec)
        }
        author_count = author_count +1
      }
    }
    
    #############################################section###########################################################
    #store section of PeerJ article was published in
    select_section <- lines[a]
    
    if (select_section == "aquatic biology"){
      section <- lines[a]
    } else if (select_section == "biochemistry biophysics molecular biology"){
      section <- "biochemistry, biophysics and molecular biology"
    } else if (select_section == "biodiversity conservation"){
      section <- lines[a]
    } else if (select_section == "bioinformatics genomics"){
      section <- "bioinformatics and genomics"
    } else if (select_section == "brain cognition"){
      section <- "brain and cognition"
    } else if (select_section == "ecology"){
      section <- lines[a]
    } else if (select_section == "environ sci"){
      section <- "environmental science"
    } else if (select_section == "microbiology"){
      section <- lines[a]
    } else if (select_section == "paleontology evolutionary science"){
      section <- lines[a]
    } else if (select_section == "plant biology"){
      section <- lines[a]
    } else if (select_section == "zoological science"){
      section <- lines[a]
    } else {
      section <- "NA"
    }
    
    ############################################Timelapse##########################################################
    dates <- vector()
    days <- 0
    
    x <- length(lines) - 3
    lines[x]<- gsub("- submitted ", "", lines[x])
    lines[x]<- gsub(" All text and materials provided via this peer-review history page are made available under a Creative Commons Attribution License, which permits unrestricted use, distribution, and reproduction in any medium, provided the original author and source are credited.", "", lines[x])
    timelapse <- append(timelapse, lines[x])
    timelapse <- gsub(",", "", timelapse)
    
    lct <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")
    
    for (i in 1:2){
      dates[i] <- as.Date(timelapse[i], "%b %d %Y")
    }
    
    days <- as.numeric(dates[1])-as.numeric(dates[2])
    
    ############################################Creating df vectors################################################
    
    for (i in 1:length(word_count+1)){
      if (author_count > 1){
        df_link <- append(df_link, link)
        df_section <- append(df_section, section)
        df_days <- append(df_days, days)
        df_version <- append(df_version, version[i])
        df_recommendation <- append(df_recommendation, recommendation[i])
        df_word_count <- append(df_word_count, word_count[i])
        df_masked <- append(df_masked, masked[i])
        df_positive <- append(df_positive, positive[i])
        df_negative <- append(df_negative, negative[i])
      }
    }
    
  } else {
    closed_reviews = closed_reviews + 1
  } # end open/closed acces loop
} # end all_loop

#################################Creating dataframe and csv file#################################################
df <- data.frame(df_link, df_section, df_days, df_version, df_recommendation, df_word_count, df_masked, df_positive, df_negative)

options(max.print=100000)
print(open_reviews)
print(closed_reviews)

write.csv(df, file = "PeerJ_Dataset_test.csv")
