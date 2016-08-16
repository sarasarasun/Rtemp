#height dataset
h <- read.csv("GaltonFamilies.csv")

# read library into working environment
library(ggplot2)

######################################################################
#                        Basic Histogram                             #
######################################################################
hist(h$child, col = "blue")
hist(h$child, breaks = 30, col = "blue")
hist(h$child, breaks = 30, col = "blue", xlab = "Height", ylab = "Frequency", main="Children's Heights")

######################################################################
#                    ggplot2 Data Specification                      #
######################################################################
# ggplot - variations on the same data
# note the change in location of "child"
# also the use of "data=" "mapping="; and not using it
# aside from color, all produce the same plot
ggplot(h, aes(child)) + geom_histogram(binwidth = 1)
ggplot(aes(child), h) + geom_histogram(binwidth = 1) # this would be wrong syntax 0729
ggplot(h, aes(child)) + geom_histogram(binwidth=1, fill="red")
ggplot(aes(child), data = h) + geom_histogram(binwidth=1, fill = "blue")
ggplot(mapping = aes(child), h) + geom_histogram(binwidth=1, fill = "green")
ggplot(h) + geom_histogram(aes(child),binwidth=1, fill = "orange")

######################################################################
#                         more histograms                            #
######################################################################
ggplot(h, aes(child)) + geom_histogram(fill = "blue")
ggplot(h, aes(child)) + geom_histogram(fill = "blue",binwidth = 1)
ggplot(h, aes(child)) + geom_histogram(fill = "blue",binwidth = 1, alpha=0.5) # alpha seems to change transparency
ggplot(h, aes(child)) + geom_histogram(fill = "purple",binwidth = 1) + labs(x = "Height", y = "Frequency")


######################################################################
#                            line chart                              #
######################################################################
#economics is a built-in dataset with 6 variables, 478 observations
head(economics)
ggplot(economics, aes(date)) + 
  geom_line(aes(y = psavert)) + 
  geom_line(aes(y = uempmed))
#color version
ggplot(economics, aes(date)) + 
  geom_line(aes(y = psavert, colour = "psavert")) + 
  geom_line(aes(y = uempmed, colour = "uempmed"))

######################################################################
#                            point chart                             #
######################################################################
ggplot(h, aes(mother, child)) + geom_point()
ggplot(h, aes(mother, child)) + geom_point(aes(shape = gender))
ggplot(h, aes(mother, child)) + geom_point(aes(color = gender,  shape=gender))
ggplot(h, aes(mother, child, color = gender)) + geom_point(aes(shape=gender), size=3)
ggplot(h, aes(mother, child, color = gender)) + geom_point() # to make sense for myself, ggplot() creates the handle, framework, must add specific geom layer to create specific plots

#more point
ggplot(economics, aes(date)) + 
  geom_point(aes(y = psavert, colour = "psavert")) + 
  geom_point(aes(y = uempmed, colour = "uempmed"))

#line, point, smooth
#line + point
lps <- ggplot(economics, aes(date,psavert)) + geom_line(aes(colour = "psavert"))
lps
lps + stat_smooth(method=loess)
lps + stat_smooth(method=loess) + geom_point(colour = "red") 
lps + geom_point(colour = "red") +  stat_smooth(method=loess) 

######################################################################
#                              Density                               #
######################################################################
ggplot(h, aes(child)) + geom_density() + labs(x = "Height", y = "Frequency")
ggplot(h, aes(child)) + geom_density(aes(fill=factor(gender), alpha=0.4), size=1)


######################################################################
#                            Boxplot                                 #
######################################################################
h.bx <- ggplot(h, aes(factor(gender), child))
h.bx + geom_boxplot()
h.bx + geom_boxplot(outlier.colour = "red", outlier.size = 2)
h.bx + geom_boxplot(aes(fill = factor(gender)))
h.bx + geom_boxplot() + coord_flip()
h.bx + geom_boxplot() + geom_jitter()

#remove legend
h.bz <- ggplot(h, aes(factor(gender), child))
h.bz + geom_boxplot(aes(fill = factor(gender))) + theme(legend.position="none")

######################################################################
#                              Facet                                 #
######################################################################
#histogram - no color
ggplot(h, aes(child)) + geom_histogram(binwidth=1) + facet_grid(gender ~ .)
ggplot(h, aes(child)) + geom_histogram(binwidth=1) + facet_grid(. ~ gender)
#histogram - color
ggplot(h, aes(child)) + geom_histogram(aes(fill = gender),binwidth = 1) + facet_grid(gender ~ .)

#density plots
ggplot(h, aes(child)) + geom_density() + facet_grid(gender ~ .)
ggplot(h, aes(child)) + geom_density(aes(fill = gender)) + facet_grid(gender ~ .)

#point
ggplot(h, aes(mother,child)) + geom_point(aes(color = gender)) + facet_grid(gender ~ .)
ggplot(h, aes(mother,child)) + geom_point(aes(color = gender)) + facet_grid(. ~ gender)

######################################################################
#                        Coordinate System                           #    
######################################################################
h.gg <- ggplot(h, aes(child)) + geom_histogram(fill = "blue",binwidth = 1, alpha=0.5)
h.gg
h.gg + coord_flip()
h.gg + scale_y_reverse()
h.gg + scale_y_reverse() + coord_flip()

#hide grid lines
h.gg + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

######################################################################
#                        Additional Geometries                       #    
######################################################################
#density2d
h.sc <- ggplot(h, aes(mother, child)) + geom_point(aes(color = gender,  shape=gender))
h.sc
h.sc + geom_density2d()
h.sc + geom_density2d() + facet_grid(. ~ gender)
h.sc + geom_density2d() + facet_grid(gender ~ .)
ggplot(h, aes(mother, child)) + geom_density2d(aes(color = gender,  shape=gender))

######################################################################
#                               qplot                                #    
######################################################################
qplot(mother,child, data=h, color = gender)
qplot(gender, child, data = h, geom="boxplot", fill = gender)
qplot(child, data = h, fill = gender) + facet_grid(. ~ gender)

