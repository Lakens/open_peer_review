# install.packages("data.table")
# install.packages("dplyr")
# install.packages("formattable")
# install.packages("tidyr")
# install.packages("ggplot2")

library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
library(ggplot2)

options(max.print=9999999)

TRS_data <- read.csv(file = "E:\\Deze folder\\BEP Open Peer Review\\TRS_Dataset.csv")
#print(TRS_data)

PeerJ_data <- read.csv(file = "E:\\Deze folder\\BEP Open Peer Review\\PeerJ_Dataset.csv")
#print(PeerJ_data)

###################Variables table###################
varlist <- c("Link", "Section", "Days", "Version", "Recommendation", "Word count", "Masked", "Positive", "Negative")
varlist2 <- c("The url of the review","Which journal/field of study the review belongs to", "The time between submitting the initial manuscript and publication in days", "The version for which the review is written", "The recommendation that is given by either the editor(PeerJ) or the reviewer(TRS)", "The amount of words that the review contains", "If the reviewer has signed the review or not", "The amount of positive words per 1000 words", "The amount of negative words per 1000 words")
df_plot <- data.frame(varlist, varlist2)
names(df_plot)[names(df_plot) == "varlist"] <- "Variable"
names(df_plot)[names(df_plot) == "varlist2"] <- "Description"
formattable(df_plot, align = c("l", rep("l", NCOL(df_plot))), size = 14)

mean(PeerJ_data$df_days, na.rm = TRUE)
t.test(PeerJ_data$df_days, TRS_data$df_days)

#####How many articles are accepted without the version being reviewed PeerJ#####
v <- 0 
for (i in 1:length(PeerJ_data$X)){
  if ((PeerJ_data$df_version[i] == 1)&(is.na(PeerJ_data$df_version[i])==FALSE)){
    if (PeerJ_data$df_link[i]!= PeerJ_data$df_link[i-1]){
    v = v + 1
    }
  }
}
print(v)

#####How many articles are accepted without the version being reviewed TRS#####
v <- 0 
for (i in 1:(length(TRS_data$X)-1)){
  if ((TRS_data$df_version[i] == 1)&(is.na(TRS_data$df_version[i])==FALSE)){
    if (TRS_data$df_link[i]!= TRS_data$df_link[i+1]){
      v = v + 1
    }
  }
}
print(v)

#####################PeerJ days and version#####################
###in total 4308 observations are used for peerj
#peerj highest version
v_days <- vector()
v_version <- vector()
v_mean_v <- vector()
v_mean_v_5 <- vector()
v_250days <- c(1:250)
v_250days_5 <- seq(from = 2.5, to = 252.5, by = 5)

for (i in 1:length(PeerJ_data$X)) {
  if ((PeerJ_data$df_link[i] != PeerJ_data$df_link[i+1])&(is.na(PeerJ_data$df_version[i])==FALSE)&(is.na(PeerJ_data$df_days[i]) == FALSE)){
    v_days <- append(v_days, PeerJ_data$df_days[i])
    v_version <- append(v_version, PeerJ_data$df_version[i])
  }
}

for (i in 1:250){
  x <- 0
  y <- 0
  for (j in 1:length(v_days)){
    if (v_days[j] == i){
      x = x + 1
      y = y + v_version[j]
    }
  }
  if (x>0){
    v_mean_v <- append(v_mean_v, y/x)
  } else {
    v_mean_v <- append(v_mean_v, NA)
  }
}

x <- 0

v_mean_v_5 <- append(v_mean_v_5, 0)
for (i in 1:50){
  y <- 0
  for (j in 1:5){
    x <- x + 1
      if (is.na(v_mean_v[x]) == FALSE){
      y <- y + v_mean_v[x]
      }
  }
  v_mean_v_5 <- append(v_mean_v_5, y/5)
}

df_plot <- data.frame(v_250days, v_mean_v)

ggplot(df_plot, aes(x = v_250days, y = v_mean_v)) +
  geom_line()

df_plot <- data.frame(v_250days_5, v_mean_v_5)

ggplot(df_plot, aes(x = v_250days_5, y = v_mean_v_5)) +
  geom_line(color = "steelblue", size = 1.2) + 
  geom_point()

df_plot<- data.frame(v_days, v_version)
mean(v_days) # average amount of days per article

###creating table of amount of version and days
tb_version_count <- vector()
tb_days_average <- vector()
tb_amount_version <- c(1:7)

tb_version_count <- append(tb_version_count, sum(df_plot$v_version == 1, na.rm = TRUE))
tb_version_count <- append(tb_version_count, sum(df_plot$v_version == 2, na.rm = TRUE))
tb_version_count <- append(tb_version_count, sum(df_plot$v_version == 3, na.rm = TRUE))
tb_version_count <- append(tb_version_count, sum(df_plot$v_version == 4, na.rm = TRUE))
tb_version_count <- append(tb_version_count, sum(df_plot$v_version == 5, na.rm = TRUE))
tb_version_count <- append(tb_version_count, sum(df_plot$v_version == 6, na.rm = TRUE))
tb_version_count <- append(tb_version_count, sum(df_plot$v_version == 7, na.rm = TRUE))
tb_days_average <- append(tb_days_average, mean(df_plot$v_days[df_plot$v_version == 1], na.rm = TRUE))
tb_days_average <- append(tb_days_average, mean(df_plot$v_days[df_plot$v_version == 2], na.rm = TRUE))
tb_days_average <- append(tb_days_average, mean(df_plot$v_days[df_plot$v_version == 3], na.rm = TRUE))
tb_days_average <- append(tb_days_average, mean(df_plot$v_days[df_plot$v_version == 4], na.rm = TRUE))
tb_days_average <- append(tb_days_average, mean(df_plot$v_days[df_plot$v_version == 5], na.rm = TRUE))
tb_days_average <- append(tb_days_average, mean(df_plot$v_days[df_plot$v_version == 6], na.rm = TRUE))
tb_days_average <- append(tb_days_average, mean(df_plot$v_days[df_plot$v_version == 7], na.rm = TRUE))

df_Version_Days <- data.frame(tb_amount_version, tb_version_count, tb_days_average)

names(df_Version_Days)[names(df_Version_Days) == "tb_amount_version"] <- "Versions"
names(df_Version_Days)[names(df_Version_Days) == "tb_version_count"] <- "Amount"
names(df_Version_Days)[names(df_Version_Days) == "tb_days_average"] <- "Average in days"

formattable(df_Version_Days, align = c("l", rep("r", NCOL(df_Version_Days) - 1)))

###creating the Version/Days plot
ggplot(df_plot, mapping = aes(x = v_version, y = v_days)) + 
  geom_boxplot(aes(group = v_version)) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  ggtitle("Version/Days boxplot PeerJ") +
  labs(x = "Version", y= "Time in days") +
  ylim(0, 750)

#####################TRS days and version#####################
###in total 1221 observations are used for TRS
###works different from PeerJ in PeerJ the versions are listed form low to high in TRS from high to low
v_days <- vector()
v_version <- vector()
tb_amount_version <- c(1:5)

v_days <- append(v_days, TRS_data$df_days[1])
v_version <- append(v_version, TRS_data$df_version[1])
for (i in 1:length(TRS_data$X)) {
  if ((TRS_data$df_link[i] != TRS_data$df_link[i+1])&(is.na(TRS_data$df_version[i+1])==FALSE)){
    v_days <- append(v_days, TRS_data$df_days[i+1])
    v_version <- append(v_version, TRS_data$df_version[i+1])
  }
}

df_plot<- data.frame(v_days, v_version)

mean(v_days) # average amount of days per article

###creating table of amount of version and days
tb_version_count <- vector()
tb_days_average <- vector()

tb_version_count <- append(tb_version_count, sum(df_plot$v_version == 1, na.rm = TRUE))
tb_version_count <- append(tb_version_count, sum(df_plot$v_version == 2, na.rm = TRUE))
tb_version_count <- append(tb_version_count, sum(df_plot$v_version == 3, na.rm = TRUE))
tb_version_count <- append(tb_version_count, sum(df_plot$v_version == 4, na.rm = TRUE))
tb_version_count <- append(tb_version_count, sum(df_plot$v_version == 5, na.rm = TRUE))
tb_days_average <- append(tb_days_average, mean(df_plot$v_days[df_plot$v_version == 1], na.rm = TRUE))
tb_days_average <- append(tb_days_average, mean(df_plot$v_days[df_plot$v_version == 2], na.rm = TRUE))
tb_days_average <- append(tb_days_average, mean(df_plot$v_days[df_plot$v_version == 3], na.rm = TRUE))
tb_days_average <- append(tb_days_average, mean(df_plot$v_days[df_plot$v_version == 4], na.rm = TRUE))
tb_days_average <- append(tb_days_average, mean(df_plot$v_days[df_plot$v_version == 5], na.rm = TRUE))

df_Version_Days <- data.frame(tb_amount_version, tb_version_count, tb_days_average)

names(df_Version_Days)[names(df_Version_Days) == "tb_amount_version"] <- "Versions"
names(df_Version_Days)[names(df_Version_Days) == "tb_version_count"] <- "Amount"
names(df_Version_Days)[names(df_Version_Days) == "tb_days_average"] <- "Average in days"

formattable(df_Version_Days, align = c("l", rep("r", NCOL(df_Version_Days) - 1)))

print(df_Version_Days)
###end

###creating the Version/Days plot
ggplot(df_plot, mapping = aes(x = v_version, y = v_days)) + 
  geom_boxplot(aes(group = v_version)) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  ggtitle("Version/Days boxplt TRS") +
  labs(x = "Version", y= "Time in days") +
  ylim(0, 750)
###end

#########PeerJ recommendations and word count#########
v_recommendation <- vector()
v_recommendation2 <- vector()
v_word_count <- vector()
v_rec <- c("Accepted", "Minor\nrevision", "Major\nrevision", "Rejected")

for (i in 1:length(PeerJ_data$X)) {
  if (PeerJ_data$df_link[i] != PeerJ_data$df_link[i+1]){
    v_recommendation <- append(v_recommendation, PeerJ_data$df_recommendation[i])
  }
}

for (i in 1:4){
v_recommendation2 <- append(v_recommendation2, sum(v_recommendation == i, na.rm = TRUE))
}

sum(v_recommendation == 1, na.rm = TRUE)
for (i in 1:4){
v_word_count <- append(v_word_count, mean(PeerJ_data$df_word_count[PeerJ_data$df_recommendation == i], na.rm = TRUE) )
}

#Recommendation bar chart PeerJ
df_plot <- data.frame(v_rec, v_recommendation2)

ggplot(df_plot, aes(x = factor(v_rec, level = v_rec), y = v_recommendation2), fill = v_rec) + 
  geom_bar(stat = "identity", fill = "#2A9DF4") +
  ggtitle("Recommendation bar chart PeerJ") +
  theme(plot.title = element_text(size=12)) +
  labs(x = "", y= "Quantity")

#Recommendation and word count bar chart PeerJ
df_plot <- data.frame(v_rec, v_word_count)

ggplot(df_plot, aes(x = factor(v_rec, level = v_rec), y = v_word_count), fill = v_rec) + 
  geom_bar(stat = "identity", fill = "#2A9DF4") +
  ggtitle("Rec/Words bar chart PeerJ") +
  theme(plot.title = element_text(size=12)) +
  labs(x= "", y= "Word count") +
  ylim(0, 600)

##########TRS recommendations and word count##########
#head(TRS_data, 100)
v_word_count <- vector()
v_recommendation <- vector()
v_recommendation2 <- vector()
v_rec <- c("Accepted", "Minor\nrevision", "Major\nrevision", "Rejected")
x <- 1

for (i in 1:length(TRS_data$X)) {
  if (TRS_data$df_link[i] != TRS_data$df_link[i+1]){
    v_recommendation <- append(v_recommendation, TRS_data$df_recommendation[i+1])
    x <- i + 1
  }
  if ((TRS_data$df_link[i] == TRS_data$df_link[i+1])&(TRS_data$df_version[i+1] == TRS_data$df_version[x])){
    v_recommendation <- append(v_recommendation, TRS_data$df_recommendation[i+1])
  }
}

for (i in 1:4){
  v_recommendation2 <- append(v_recommendation2, sum(v_recommendation == i, na.rm = TRUE))
}

for (i in 1:4){
  v_word_count <- append(v_word_count, mean(TRS_data$df_word_count[TRS_data$df_recommendation == i], na.rm = TRUE) )
}


#Recommendation bar chart TRS
df_plot <- data.frame(v_rec, v_recommendation2)

ggplot(df_plot, aes(x = factor(v_rec, level = v_rec), y = v_recommendation2), fill = v_rec) + 
  geom_bar(stat = "identity", fill = "#2A9DF4") +
  ggtitle("Recommendation bar chart TRS") +
  theme(plot.title = element_text(size=12)) +
  labs(x = "", y= "Quantity")
  

#Recommendation and word count bar chart TRS
df_plot <- data.frame(v_rec, v_word_count)

ggplot(df_plot, aes(x = factor(v_rec, level = v_rec), y = v_word_count), fill = v_rec) + 
  geom_bar(stat = "identity", fill = "#2A9DF4") +
  ggtitle("Rec/Words bar chart TRS") +
  theme(plot.title = element_text(size=12)) +
  labs(x= "", y= "Word count") +
  ylim(0, 600)

t.test(PeerJ_data$df_word_count, TRS_data$df_word_count)

###################Masked Peerj/TRS###################
library(ggplot2)
library(formattable)

### general numbers
sum(PeerJ_data$df_masked == 0)
sum(PeerJ_data$df_masked == 1)
sum(TRS_data$df_masked == 0)
sum(TRS_data$df_masked == 1)

v_masked <- vector()
v_plot <- c("Unmasked PeerJ","Masked PeerJ","Unmasked TRS","Masked TRS")
v_plot2 <- c("Accepted", "Minor\nrevision", "Major\nrevision", "Rejected")

v_masked <- append(v_masked, mean(PeerJ_data$df_word_count[PeerJ_data$df_masked == 0], na.rm = TRUE))
v_masked <- append(v_masked, mean(PeerJ_data$df_word_count[PeerJ_data$df_masked == 1], na.rm = TRUE))
v_masked <- append(v_masked, mean(TRS_data$df_word_count[TRS_data$df_masked == 0], na.rm = TRUE))
v_masked <- append(v_masked, mean(TRS_data$df_word_count[TRS_data$df_masked == 1], na.rm = TRUE))

df_plot <- data.frame(v_plot, v_masked)

ggplot(df_plot, aes(x = factor(v_plot, level = v_plot), y = v_masked)) + 
  geom_bar(stat = "identity", width = 0.8, fill = "#2A9DF4") +
  ggtitle("Masked/Words bar chart") +
  theme(plot.title = element_text(size=12)) +
  labs(x = "", y= "Word count", caption = "Figure 3") +
  ylim(0, 500)

names(df_plot)[names(df_plot) == "v_plot"] <- "Masked/Unmasked"
names(df_plot)[names(df_plot) == "v_masked"] <- "Average word count"
formattable(df_plot, align = c("l", rep("r", NCOL(df_plot) - 1)))

v_rec_masked <- vector()
v_rec_masked <- append(v_rec_masked, sum(PeerJ_data$df_recommendation == 1 & PeerJ_data$df_masked == 1, na.rm = TRUE))
v_rec_masked <- append(v_rec_masked, sum(PeerJ_data$df_recommendation == 2 & PeerJ_data$df_masked == 1, na.rm = TRUE))
v_rec_masked <- append(v_rec_masked, sum(PeerJ_data$df_recommendation == 3 & PeerJ_data$df_masked == 1, na.rm = TRUE))
v_rec_masked <- append(v_rec_masked, sum(PeerJ_data$df_recommendation == 4 & PeerJ_data$df_masked == 1, na.rm = TRUE))

df_plot <- data.frame(v_plot2, v_rec_masked)

ggplot(df_plot, aes(x = factor(v_plot2, level = v_plot2), y = v_rec_masked)) + 
  geom_bar(stat = "identity", width = 0.8, fill = "#2A9DF4") +
  ggtitle("PeerJ recommendations masked") +
  theme(plot.title = element_text(size=12)) +
  labs(x = "", y= "Quantity")

v_rec_unmasked <- vector()
v_rec_unmasked <- append(v_rec_unmasked, sum(PeerJ_data$df_recommendation == 1 & PeerJ_data$df_masked == 0, na.rm = TRUE))
v_rec_unmasked <- append(v_rec_unmasked, sum(PeerJ_data$df_recommendation == 2 & PeerJ_data$df_masked == 0, na.rm = TRUE))
v_rec_unmasked <- append(v_rec_unmasked, sum(PeerJ_data$df_recommendation == 3 & PeerJ_data$df_masked == 0, na.rm = TRUE))
v_rec_unmasked <- append(v_rec_unmasked, sum(PeerJ_data$df_recommendation == 4 & PeerJ_data$df_masked == 0, na.rm = TRUE))

df_plot <- data.frame(v_plot2, v_rec_unmasked)

ggplot(df_plot, aes(x = factor(v_plot2, level = v_plot2), y = v_rec_unmasked)) + 
  geom_bar(stat = "identity", width = 0.8, fill = "#2A9DF4") +
  ggtitle("PeerJ recommendations unmasked") +
  theme(plot.title = element_text(size=12)) +
  labs(x = "", y= "Quantity")

v_rec_masked <- vector()
v_rec_masked <- append(v_rec_masked, sum(TRS_data$df_recommendation == 1 & TRS_data$df_masked == 1, na.rm = TRUE))
v_rec_masked <- append(v_rec_masked, sum(TRS_data$df_recommendation == 2 & TRS_data$df_masked == 1, na.rm = TRUE))
v_rec_masked <- append(v_rec_masked, sum(TRS_data$df_recommendation == 3 & TRS_data$df_masked == 1, na.rm = TRUE))
v_rec_masked <- append(v_rec_masked, sum(TRS_data$df_recommendation == 4 & TRS_data$df_masked == 1, na.rm = TRUE))

df_plot <- data.frame(v_plot2, v_rec_masked)

ggplot(df_plot, aes(x = factor(v_plot2, level = v_plot2), y = v_rec_masked)) + 
  geom_bar(stat = "identity", width = 0.8, fill = "#2A9DF4") +
  ggtitle("TRS recommendations masked") +
  theme(plot.title = element_text(size=12)) +
  labs(x = "", y= "Quantity")

v_rec_unmasked <- vector() 
v_rec_unmasked <- append(v_rec_unmasked, sum(TRS_data$df_recommendation == 1 & TRS_data$df_masked == 0, na.rm = TRUE))
v_rec_unmasked <- append(v_rec_unmasked, sum(TRS_data$df_recommendation == 2 & TRS_data$df_masked == 0, na.rm = TRUE))
v_rec_unmasked <- append(v_rec_unmasked, sum(TRS_data$df_recommendation == 3 & TRS_data$df_masked == 0, na.rm = TRUE))
v_rec_unmasked <- append(v_rec_unmasked, sum(TRS_data$df_recommendation == 4 & TRS_data$df_masked == 0, na.rm = TRUE))

df_plot <- data.frame(v_plot2, v_rec_unmasked)

ggplot(df_plot, aes(x = factor(v_plot2, level = v_plot2), y = v_rec_unmasked)) + 
  geom_bar(stat = "identity", width = 0.8, fill = "#2A9DF4") +
  ggtitle("TRS recommendations unmasked") +
  theme(plot.title = element_text(size=12)) +
  labs(x = "", y= "Quantity")

##############Positive/Negative PeerJ#################
v_pos_neg <- vector()
v_p_n <- c("Positive words unmasked","Positive words masked","Negative words unmasked","Negative words masked")

pos_avg_1000 <- (mean(PeerJ_data$df_positive[PeerJ_data$df_masked == 0])/mean(PeerJ_data$df_word_count[PeerJ_data$df_masked == 0])*1000) 
v_pos_neg <- append(v_pos_neg, pos_avg_1000)

pos_avg_1000 <- (mean(PeerJ_data$df_positive[PeerJ_data$df_masked == 1])/mean(PeerJ_data$df_word_count[PeerJ_data$df_masked == 1])*1000) 
v_pos_neg <- append(v_pos_neg, pos_avg_1000)

pos_avg_1000 <- (mean(PeerJ_data$df_negative[PeerJ_data$df_masked == 0])/mean(PeerJ_data$df_word_count[PeerJ_data$df_masked == 0])*1000) 
v_pos_neg <- append(v_pos_neg, pos_avg_1000)

pos_avg_1000 <- (mean(PeerJ_data$df_negative[PeerJ_data$df_masked == 1])/mean(PeerJ_data$df_word_count[PeerJ_data$df_masked == 1])*1000) 
v_pos_neg <- append(v_pos_neg, pos_avg_1000)

df_plot <- data.frame(v_p_n, v_pos_neg)

ggplot(df_plot, aes(x = factor(v_p_n, level = v_p_n), y = v_pos_neg)) + 
  geom_bar(stat = "identity", fill = "#2A9DF4") +
  ggtitle("Masked/Words bar chart") +
  theme(plot.title = element_text(size=12)) +
  labs(x = "", y= "Word count")
names(df_plot)[names(df_plot) == "v_p_n"] <- "PeerJ positive/negative words"
names(df_plot)[names(df_plot) == "v_pos_neg"] <- "Average per 1000 words"
formattable(df_plot, align = c("l", rep("r", NCOL(df_plot) - 1)))

t.test(PeerJ_data$df_negative[PeerJ_data$df_masked == 1], PeerJ_data$df_negative[PeerJ_data$df_masked == 0])
t.test(PeerJ_data$df_positive[PeerJ_data$df_masked == 1], PeerJ_data$df_positive[PeerJ_data$df_masked == 0])
t.test(TRS_data$df_negative[TRS_data$df_masked == 1], TRS_data$df_negative[TRS_data$df_masked == 0])
t.test(TRS_data$df_positive[TRS_data$df_masked == 1], TRS_data$df_positive[TRS_data$df_masked == 0])
##############Positive/Negative TRS###################
v_pos_neg <- vector()
v_p_n <- c("Positive words unmasked","Positive words masked","Negative words unmasked","Negative words masked")

pos_avg_1000 <- (mean(TRS_data$df_positive[TRS_data$df_masked == 0])/mean(TRS_data$df_word_count[TRS_data$df_masked == 0])*1000) 
v_pos_neg <- append(v_pos_neg, pos_avg_1000)

pos_avg_1000 <- (mean(TRS_data$df_positive[TRS_data$df_masked == 1])/mean(TRS_data$df_word_count[TRS_data$df_masked == 1])*1000) 
v_pos_neg <- append(v_pos_neg, pos_avg_1000)

pos_avg_1000 <- (mean(TRS_data$df_negative[TRS_data$df_masked == 0])/mean(TRS_data$df_word_count[TRS_data$df_masked == 0])*1000) 
v_pos_neg <- append(v_pos_neg, pos_avg_1000)

pos_avg_1000 <- (mean(TRS_data$df_negative[TRS_data$df_masked == 1])/mean(TRS_data$df_word_count[TRS_data$df_masked == 1])*1000) 
v_pos_neg <- append(v_pos_neg, pos_avg_1000)

df_plot <- data.frame(v_p_n, v_pos_neg)

ggplot(df_plot, aes(x = factor(v_p_n, level = v_p_n), y = v_pos_neg)) + 
  geom_bar(stat = "identity", fill = "#2A9DF4") +
  ggtitle("Masked/Words bar chart") +
  theme(plot.title = element_text(size=12)) +
  labs(x = "", y= "Word count")


names(df_plot)[names(df_plot) == "v_p_n"] <- "TRS positive/negative words"
names(df_plot)[names(df_plot) == "v_pos_neg"] <- "Average per 1000 words"
formattable(df_plot, align = c("l", rep("r", NCOL(df_plot) - 1)))

