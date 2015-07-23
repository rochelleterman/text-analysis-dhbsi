# Sentiment Analysis

rm(list=ls())
setwd("~/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/text-analysis-dhbsi")

library(ggplot2)
library(scales)
library(data.table)
library(qdap)

##################
##### HAMLET #####
##################

ham.dat <- data.table(hamlet)
# add word counts
ham.dat[, wc := wc(dialogue,missing=0)]
# Add cumulative word count and percent complete as proxy for progression
ham.dat[, cumsum := cumsum(wc)]
ham.dat[, pct.complete := ham.dat$cumsum / sum(ham.dat$wc)]
ham.dat[, pct.complete.100 := pct.complete * 100]

# calculate polarity
poldat.ham <- with(hamlet, polarity(dialogue, act, constrain = TRUE))
counts(poldat.ham)[1:10,]
polcount.ham <- na.omit(counts(poldat.ham)$polarity) # get vector of just the polarity scores

# put it into a data frame
len.ham <- length(polcount.ham)
pol.df.ham <- data.frame(polarity = polcount.ham, Time=1:len.ham)

############
### RAJ ####
############

raj.dat <- data.table(rajSPLIT)
# add word counts
raj.dat[, wc := wc(dialogue,missing=0)]
# Add cumulative word count and percent complete as proxy for progression
raj.dat[, cumsum := cumsum(wc)]
raj.dat[, pct.complete := raj.dat$cumsum / sum(raj.dat$wc)]
raj.dat[, pct.complete.100 := pct.complete * 100]

# calculate polarity
poldat.raj <- with(rajSPLIT, polarity(dialogue, act, constrain = TRUE))
counts(poldat.raj)[1:10,]
polcount.raj <- na.omit(counts(poldat.raj)$polarity) # get vector of just the polarity scores

# put it into a data frame
len <- length(polcount.raj)
pol.df.raj <- data.frame(polarity = polcount.raj, Time=1:len)

##############
#### Plot ####
##############

## Calculate background rectangles for RAJ
ends <- cumsum(rle(counts(poldat.raj)$act)$lengths)
ends
starts <- c(1, head(ends + 1, -1))
starts
rects <- data.frame(xstart = starts, xend = ends + 1,
                    Act = c("I", "II", "III", "IV", "V"))

# Plot RAJ

ggplot() + theme_bw() +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
                              ymin = -Inf, ymax = Inf, fill = Act), alpha = 0.17) +
  geom_line(data = pol.df.raj, aes(y=polarity, x = Time), size=1, color = "grey60") +
  geom_smooth(data = pol.df.raj, aes(y=polarity, x = Time), color="royalblue", fill="lightgray", size=1.4) +
  geom_hline(y=mean(polcount.raj), color="grey30", size=1, alpha=.3, linetype=2) +
  annotate("text", x = mean(ends[1:2]), y = mean(polcount.raj), color="grey30",
           label = "Average Polarity", vjust = .3, size=3) +
  ylab("Average Polarity") + xlab("Duration") +
  scale_x_continuous(expand = c(0,0)) +
  geom_text(data=rects, aes(x=(xstart + xend)/2, y=-.04,
                            label=paste("Act", Act)), size=3) +
  guides(fill=FALSE) +
  scale_fill_brewer(palette="Set1")


# plot HAMLET + RAJ
ggplot() +
  geom_smooth(data = pol.df.raj, aes(y=polarity, x = Time, color="Romeo and Juliet", fill="Romeo and Juliet"), size=1.4) +
  geom_smooth(data = pol.df.ham, aes(y=polarity, x = Time, color="Hamlet", fill="Hamlet"), size=1.4) +
  geom_hline(y=mean(polcount.raj), color="gray30", size=1, alpha=.3, linetype=2) +
  annotate("text", x = mean(ends[1:2]), y = mean(polcount.raj), color="grey30",
           label = "Average Polarity - RAJ", vjust = .3, size=3) +
  geom_hline(y=mean(polcount.ham), color="gray30", size=1, alpha=.3, linetype=2) +
  annotate("text", x = mean(ends[1:2]), y = mean(polcount.ham), color="grey30",
           label = "Average Polarity - HAM", vjust = .3, size=3) +
  ylab("Average Polarity") + xlab("Duration") +
  scale_x_continuous(expand = c(0,0)) +
  geom_text(data=rects, aes(x=(xstart + xend)/2, y=-.04,
                           label=paste("Act", Act)), size=3) +
  guides(fill=F) +
  scale_fill_brewer(palette="Set1") +
  scale_colour_manual('Legend', breaks = c('Romeo and Juliet', 'Hamlet'),
                      values = c('red', 'royalblue'))


data <- data.frame(a,b,c,d,e,f,g,h)

##################
### Thriller ####
##################

# read
thriller <- read.csv("Data/thriller.csv")
# split
thril.split <- sentSplit(thriller, "Lyrics")
# polarity
(poldat <- with(thril.split, polarity(Lyrics, Song)))
# have a peak
counts(poldat)[1:10,]
# plot
plot(poldat)
x <- plot(poldat)
x$p2

