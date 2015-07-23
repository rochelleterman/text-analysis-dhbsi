# based on: http://anythingbutrbitrary.blogspot.com/2014/01/statistics-meets-rhetoric-text-analysis.html

library(ggplot2)
library(scales)
library(data.table)
library(qdap)

rm(list=ls())

raj.dat <- data.table(rajSPLIT)
# add word counts
raj.dat[, wc := wc(dialogue,missing=0)]
# Add cumulative word count and percent complete as proxy for progression
raj.dat[, cumsum := cumsum(wc)]
raj.dat[, pct.complete := raj.dat$cumsum / sum(raj.dat$wc)]
raj.dat[, pct.complete.100 := pct.complete * 100]

# calculate polarity
poldat <- with(rajSPLIT, polarity(dialogue, act, constrain = TRUE))
counts(poldat)[1:10,]
polcount <- na.omit(counts(poldat)$polarity) # get vector of just the polarity scores

# put it into a data frame
len <- length(polcount)
po.df <- data.frame(polarity = polcount, Time=1:len)

## Calculate background rectangles
ends <- cumsum(rle(counts(poldat)$act)$lengths)
starts <- c(1, head(ends + 1, -1))
rects <- data.frame(xstart = starts, xend = ends + 1,
                    Act = c("I", "II", "III", "IV", "V"))

# plot
ggplot() + theme_bw() +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
                              ymin = -Inf, ymax = Inf, fill = Act), alpha = 0.17) +
  geom_line(data = cumpolarity, aes(y=polarity, x = Time), size=1, color = "grey60") +
  geom_smooth(data = cumpolarity, aes(y=polarity, x = Time), color="royalblue", fill="lightgray", size=1.4) +
  geom_hline(y=mean(polcount), color="grey30", size=1, alpha=.3, linetype=2) +
  annotate("text", x = mean(ends[1:2]), y = mean(polcount), color="grey30",
           label = "Average Polarity", vjust = .3, size=3) +
  ylab("Average Polarity") + xlab("Duration") +
  scale_x_continuous(expand = c(0,0)) +
  geom_text(data=rects, aes(x=(xstart + xend)/2, y=-.04,
                            label=paste("Act", Act)), size=3) +
  guides(fill=FALSE) +
  scale_fill_brewer(palette="Set1")

# zoom

ggplot() + theme_bw() +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
                              ymin = -Inf, ymax = Inf, fill = Act), alpha = 0.17) +
  #geom_line(data = cumpolarity, aes(y=polarity, x = Time), size=1, color = "grey60") +
  geom_smooth(data = cumpolarity, aes(y=polarity, x = Time), color="royalblue", fill="lightgray", size=1.4) +
  geom_hline(y=mean(polcount), color="grey30", size=1, alpha=.3, linetype=2) +
  annotate("text", x = mean(ends[1:2]), y = mean(polcount), color="grey30",
           label = "Average Polarity", vjust = .3, size=3) +
  ylab("Average Polarity") + xlab("Duration") +
  scale_x_continuous(expand = c(0,0)) +
  geom_text(data=rects, aes(x=(xstart + xend)/2, y=-.04,
                            label=paste("Act", Act)), size=3) +
  guides(fill=FALSE) +
  scale_fill_brewer(palette="Set1")

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


