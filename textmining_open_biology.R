###Notes###
library(dplyr)
library(tidytext)
library(tm)
library(stringr)
options(max.print=9999999) #allows the console to output more lines

#########################################variables used to create the dataframe#################################
df_link <- c()
df_section <- c()
df_days <- c()
df_version <- c()
df_recommendation <- c()
df_word_count <- c()
df_masked <- c()
df_reviewer_name <- c()
df_reviewer_number <- c()
df_id <- c()

#########################################Read txt id##############################################################
article_id <- vector()

path <- "royal_society_pdf_files/OB_pdf_list.txt"  #the list of non corrupted Open Biology ID's 

conn <- file(path,open="r")
article_id <- readLines(conn)
close(conn)


###########################################Read txt file##########################################################
for (r in 1:length(article_id)){
  
  review_id <- article_id[r]

  path <- paste("royal_society_txt_files/open_biology/review", review_id, ".txt", sep="") #used for Open Biology
  #path <- paste("E:\\Deze folder\\\\BEP Open Peer Review\\TXT_Files\\TRS\\Open Science\\Review", review_id, ".txt", sep="") #used for Open Science
  
  if (file.exists(path)){
  open_file <- file(path,open="r")
  lines <- readLines(open_file)
  lines <- trimws(lines)
  
  close(open_file)

                                           ######variables######

###########################Masked, Version, recommendation, days and word count###################################
    
    recommendation <- vector()
    recommendation_count <- 1
    masked <- vector()
    author_count <- 1
    version <- vector()
    version_count <- 1
    version_count2 <- 0  # used to fix a problem when version_count goes higher than the actual versions of the manuscript
    timelapse <- vector()
    dates <- vector()
    days <- 0
    positive <- vector()
    negative <- vector()
    switch_link <- 1
    reviewer_name <- vector() #store reviewer names
    reviewer_number <- vector() #store reviewer number
    
    word_count <- vector()  #Vector that contains the wordcounts
    review_count <- 1       #Counts which review is being counted
    switch <- 0
    x <- 0                  #counter for word_count position
    
    
    lct <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")

    
    for (i in 1:length(lines)){
      
      if ((switch == 1)&(lines[i] == "label_end_comment")) {
        switch <- 0
      }
      
      if ((switch == 1)&(str_sub(lines[i], 1,6) == "label_" )) {
        switch <- 0
      }
      
      if (switch == 1) {
        word_count[x] <- word_count[x] + sapply(gregexpr("[[:alnum:]]+", lines[i]), function(x) sum(x > 0))
        
        # sep_word <- strsplit(lines[i], " ")
        # sep_word <- unlist(sep_word)
        # 
        # d <- data.frame(sep_word, stringsAsFactors = FALSE)
        # 
        # for (j in 1:length(d$sep_word)){
        #   d$sep_word[j] <- gsub(",", "", d$sep_word[j])
        #   d$sep_word[j] <- gsub("\\.", "", d$sep_word[j])
        #   
        #   for (k in 1:length(nrc_negative)){
        #     if (d$sep_word[j] == nrc_negative[k]){
        #       negative[x] = negative[x] + 1
        #     }
        #   }
        #   
        #   for (l in 1:length(nrc_positive)){
        #     if (d$sep_word[j] == nrc_positive[l]){
        #       positive[x] = positive[x] + 1
        #     }
        #   }
        # }
      }
      if(lines[i] == paste("label_comment_", recommendation_count - 1, sep="")) {
        switch <- 1
        x = x + 1
        review_count = review_count + 1
        word_count[x] <- 0
        # positive[x] <- 0
        # negative[x] <- 0
        # 
      }
      
      if (grepl("http://dx.doi.org", lines[i])&(switch_link == 1)){
        switch_link <- 0
        link <- lines[i]
      }
      
      if (lines[i] == paste("label_version_", version_count, sep="")){
        version_count <- version_count +1
      }
      
      if(lines[i] == paste("label_recommendation_", recommendation_count, sep="")){
        if (str_sub(lines[i+1], 1, 12)  == "Accept as is"){
          recommendation <- append(recommendation, 1)
          version <- append(version, version_count - 1)
          version_count2 <- version_count
        }
        
        if (str_sub(lines[i+1], 1, 19)  == "Accept in principle"){
          recommendation <- append(recommendation, 1)
          version <- append(version, version_count - 1)
          version_count2 <- version_count
        }
        
        if (str_sub(lines[i+1], 1, 26)  == "Accept with minor revision"){
           recommendation <- append(recommendation, 2)
           version <- append(version, version_count - 1)
           version_count2 <- version_count
        }
        
        if (str_sub(lines[i+1], 1, 14)  == "Major revision"){
           recommendation <- append(recommendation, 3)
           version <- append(version, version_count - 1)
           version_count2 <- version_count
        }
        
        if (str_sub(lines[i+1], 1, 6)  == "Reject"){
          recommendation <- append(recommendation, 4)
          version <- append(version, version_count - 1)
          version_count2 <- version_count
        }
        recommendation_count = recommendation_count +1
      }
      
      if (lines[i] == paste("label_author_", author_count, sep="")){
        
        #reviewer_name <- append(reviewer_name, gsub("Review form: ", "", lines[i+1])) #Save author name (or Reviewer name)
        reviewer_name <- append(reviewer_name, str_sub(lines[i+1], 26, str_length(lines[i+1])-1))  #Save author name (or Reviewer name)
        reviewer_number <- append(reviewer_number, str_sub(lines[i+1], 23, 23))  #Save author name (or Reviewer name)
        
        if (str_sub(lines[i+1], 14, 24) == "Reviewer 1"){
          masked <- append(masked, 1)
        } else if (str_sub(lines[i+1], 14, 24) == "Reviewer 2"){
          masked <- append(masked, 1)
        } else if (str_sub(lines[i+1], 14, 24) == "Reviewer 3"){
          masked <- append(masked, 1)
        } else {
          masked <- append(masked, 0)
        }
        author_count = author_count + 1
      }
      
      if (str_sub(lines[i], 1 , 20) == "Original submission:"){
        lines[i]<- gsub("Original submission: ", "", lines[i])
        lines[i]<- gsub(" Note: Reports are unedited and appear as", "", lines[i])
        timelapse <- append(timelapse, lines[i])
      }
      
      if (str_sub(lines[i], 1, 16)  == "Final acceptance"){
        lines[i]<- gsub("Final acceptance: ", "", lines[i])
        lines[i]<- gsub(" appears in chronological order.", "", lines[i])
        timelapse <- append(timelapse, lines[i])
      }
  }
    
  for (i in length(version):1){
    version[i] <- version_count2 - version[i]
  }
    
  for (i in 1:2){
    dates[i] <- as.Date(timelapse[i], "%d %B %Y")
  }
  
  days <- dates[2]-dates[1]
  section <- "Open Biology" #used for open biology
  #section <- "Open Science" #used for open science

##############################################Data frame #####################################################  
    if (length(word_count>=1)){
      for (i in 1:length(word_count + 1)){
        df_link <- append(df_link, link)
        df_section <- append(df_section, section)
        df_days <- append(df_days, days)
        df_version <- append(df_version, version[i])
        df_recommendation <- append(df_recommendation, recommendation[i])
        df_word_count <- append(df_word_count, word_count[i])
        df_masked <- append(df_masked, masked[i])
        df_reviewer_name <- append(df_reviewer_name, reviewer_name[i])
        df_reviewer_number <- append(df_reviewer_number, reviewer_number[i])
      }
    }
  }
}#end all_loop

df <- data.frame(df_link, df_section, df_days, df_version, df_recommendation, df_word_count, df_masked, df_reviewer_name, df_reviewer_number)

OB_df <- df                                                #used to store Open Biology df 
#OS_df <- df                                               #used to store Open Science df
#total_df <- rbind(OS_df, OB_df)                           #used to combine the two df's
saveRDS(df, file = "royal_society_data_ob.rds")             #used to create the csv file
    