library(ggplot2)
#need gplots for color function
library(gplots)

dfSub <- read.csv('dataset-meetup11.csv')

#colors
nRegions <- length(unique(dfSub$Region))

#color schemes
regionColors <- rich.colors(nRegions)

#theme
theme_set(theme_bw())
#theme_set(theme_classic())

#base data layer
p <- ggplot(dfSub, aes(x=Biz, y=Tech))

#defauly color settings
g1 <- p + geom_point(aes(size=Members, color=Region))
g1

#rich colors
g2 <- p + geom_point(aes(size=Members, color=Region)) + scale_colour_manual(values=regionColors)
g2
#add text labels
g2 + geom_text(aes(label = Region), size=3,hjust=1, vjust=1) + xlim(c(0.02, 0.17))

#scale adjustment
g3 <- p + geom_point(aes(size=Members, color=Region)) +
     scale_colour_manual(values=regionColors) +
     scale_x_continuous(expand=c(0.1,0)) +
     scale_y_continuous(expand=c(0.1,0)) +
     scale_size_continuous(range=c(2,20))
g3
#change scaling factor from 0.1 to 0.2

#themes
library(ggthemes)
g4 <- p + geom_point(aes(size=Members, color=Region)) +
  scale_x_continuous(expand=c(0.2,0)) +
  scale_size_continuous(range=c(5,20)) + scale_fill_excel('fill') + theme_excel()
g4

g5 <- p + geom_point(aes(size=Members, color=Region)) +
  scale_x_continuous(expand=c(0.1,0)) +
  scale_size_continuous(range=c(5,20)) + theme_economist(dkpanel=TRUE) + scale_colour_economist(stata=TRUE)
g5
