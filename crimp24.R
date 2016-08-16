# crimp data set
crimp24 <- read.csv("all_Crimp data h24.csv")

# read library into working environment
library(ggplot2)

# some histograms
# hist of connector pnp speed
ggplot(crimp24, aes(Vconn)) + geom_histogram(fill = "blue",binwidth = 0.005, alpha=0.5) # alpha seems to change transparency
ggplot(crimp24, aes(Vconn)) + geom_histogram(fill = "purple", binwidth = 0.005) + labs(x = "Speed", y = "Frequency")

# this one will not work ... why???
ggplot(crimp24, aes(Vconn)) + geom_histogram(binwidth = 0.005, alpha=0.5, fill = factor(PN)) # alpha seems to change transparency
# try the density one instead
ggplot(crimp24, aes(Vconn)) + geom_density(aes(fill=factor(PN), alpha=0.4), size=1)

#histogram - color
ggplot(crimp24, aes(Vconn)) + geom_histogram(aes(fill = PN),binwidth = 0.005) + facet_grid(PN ~ .)


# some time series & line charts
head(crimp24)
# this is not working, time is not right
ggplot(crimp24, aes(Time)) + 
  geom_line(aes(y = Pre3)) + 
  geom_line(aes(y = Fin3))


# some point charts

ggplot(crimp24, aes(Pre3, Fin3)) + geom_point()
ggplot(crimp24, aes(Pre3, Fin3)) + geom_point(aes(shape = PN))
ggplot(crimp24, aes(Pre3, Fin3)) + geom_point(aes(color = PN,  shape=PN))
ggplot(crimp24, aes(Pre3, Fin3)) + geom_point(aes(color = PN,  shape=PN, alpha=0.5))
ggplot(crimp24, aes(Pre3, Fin3, color = PN)) + geom_point(aes(shape=PN), size=3)
ggplot(crimp24, aes(Pre3, Fin3, color = PN)) + geom_point() # to make sense for myself, ggplot() creates the handle, framework, must add specific geom layer to create specific plots

#line, point, smooth
#line + point
lps <- ggplot(crimp24, aes(Pre3, Fin3)) + geom_point()
lps
lps + stat_smooth(method=loess)
lps + stat_smooth(method=loess) + geom_point(colour = "red") 
lps + geom_point(colour = "red") +  stat_smooth(method=loess) 

