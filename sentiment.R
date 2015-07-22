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
pol.df <- polarity(rajSPLIT$dialogue)$all
raj.dat[, words := pol.df$wc]
raj.dat[, pol := pol.df$polarity]

with(raj.dat, plot(pct.complete, pol))

my.theme <- 
  theme(plot.background = element_blank(), # Remove background
        panel.grid.major = element_blank(), # Remove gridlines
        panel.grid.minor = element_blank(), # Remove more gridlines
        panel.border = element_blank(), # Remove border
        panel.background = element_blank(), # Remove more background
        axis.ticks = element_blank(), # Remove axis ticks
        axis.text=element_text(size=14), # Enlarge axis text font
        axis.title=element_text(size=16), # Enlarge axis title font
        plot.title=element_text(size=24, hjust=0)) # Enlarge, left-align title

CustomScatterPlot <- function(gg)
  return(gg + geom_point(color="grey60") + # Lighten dots
           stat_smooth(color="royalblue", fill="lightgray", size=1.4) + 
           xlab("Percent complete (by word count)") + 
           scale_x_continuous(labels = percent) + my.theme)

CustomScatterPlot(ggplot(raj.dat, aes(pct.complete, pol)) +
                    ylab("Sentiment (sentence-level polarity)") + 
                    ggtitle("Sentiment of Romeo and Juliet"))

# zoom in
CustomScatterPlot(ggplot(raj.dat, aes(pct.complete, pol)) +
                    ylab("Sentiment (sentence-level polarity)") + 
                    ggtitle("Sentiment of Romeo and Juliet")) +
  coord_cartesian(ylim = c(-.3, .3))

