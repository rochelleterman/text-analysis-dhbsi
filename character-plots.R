rm(list=ls())
setwd("~/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/text-analysis-dhbsi")

library(qdap) # quantiative discourse analysis of transcripts
library(ggplot2) # plotting discourse data

# load data
alpha <- read.csv("Data/alphabet-aerobics.csv",header = F)
names(alpha) <- c("line","text")

# see a truncated version of the data
head(truncdf(alpha),10)

# make character table
char <- character_table(alpha$text, alpha$line)

# refactor so it goes in sequential order

char$raw$line <- factor(1:51)
char$rnp$line <- factor(1:51)
char$prop$line <- factor(1:51)

# get rid of punctuation
char$prop <- char$prop[,8:33]

# alter labels
pl <- plot(char)
pl$labels$char <- "line"
pl + ggtitle("Distribution of letters in Blackalicious' 'Alphabet Aerobics' ")
