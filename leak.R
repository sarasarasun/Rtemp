leak <- read.csv('leak-rate.csv')
# read library into working environment
library(ggplot2)

######################################################################
#                            point chart                             #
######################################################################
ggplot(leak, aes(leak$Part, leak$LeakRate)) + geom_point()
ggplot(leak, aes(leak$Part, leak$LeakRate)) + geom_point(aes(shape = SEA))
ggplot(leak, aes(leak$Part, leak$LeakRate)) + geom_point(aes(color = SEA,  shape=SEA),size=3)
ggplot(leak, aes(leak$Part, leak$LeakRate)) + geom_point(aes(color = SEA,  shape=SEA),size=3) + 
  facet_grid(. ~ SEA) +
  theme(legend.position="none")
ggplot(leak, aes(leak$Part, leak$LeakRate)) + geom_point(aes(color = SEA,  shape=SEA),size=3,alpha=0.4)
#ggplot(h, aes(mother, child, color = gender)) + geom_point(aes(shape=gender), size=3)

logleak <- log10(leak$LeakRate)
leakaug <- data_frame(leak, logleak)
ggplot(leak, aes(leak$Part, logleak)) + geom_point(aes(color = SEA,  shape=SEA),size=2) + 
  facet_grid(. ~ SEA) +
  theme(legend.position="none")

#more point
ggplot(economics, aes(date)) + 
  geom_point(aes(y = psavert, colour = "psavert")) + 
  geom_point(aes(y = uempmed, colour = "uempmed"))
