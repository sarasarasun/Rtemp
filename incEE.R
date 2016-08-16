######################################################################
#                                                                    #
#                        Basic Conditioning                          #
######################################################################

inc <- read.csv('return47529808INCdata.csv')

######################################################################
#                              save good                             #
######################################################################

######################################################################
#               cleansing data for neater analysis                   #
######################################################################
# separate goods and bad
good <- inc[grep("good",inc$Status,ignore.case = T,),]
ret <- inc[inc$SerialNo==47529808,] # this is the return

######################################################################
#                              Density                               #
######################################################################

library(ggplot2)

# Vcc
tVcc <- paste("Vcc of Return: ", ret$Vcc)
hisVcc <- ggplot(good, aes(good$Vcc)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.00015,
                 colour="black", fill="white")
densVcc <- hisVcc + geom_density(alpha=0.2, fill="#FF6666") +    # Overlay density
  theme(legend.position = "none") +
  labs(x = "Vcc", title = tVcc)
densVcc + geom_vline(aes(xintercept=ret$Vcc),   
                     color="red", linetype="dashed", size=1)

# Icc
tIcc <- paste("Icc of Return: ", ret$Icc)
hisIcc <- ggplot(good, aes(good$Icc)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 colour="black", fill="white")
densIcc <- hisIcc + geom_density(alpha=0.2, fill="#FF6666") +    # Overlay density
  theme(legend.position = "none") +
  labs(x = "Icc", title = tIcc)
densIcc + geom_vline(aes(xintercept=ret$Icc),   
                     color="red", linetype="dashed", size=1)

# Capacitance
tCap <- paste("Capacitance of Return: ", ret$Capacitance)
hisCap <- ggplot(good, aes(good$Capacitance)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 colour="black", fill="white")
densCap <- hisCap + geom_density(alpha=0.2, fill="#FF6666") +    # Overlay density
  theme(legend.position = "none") +
  labs(x = "Capacitance", title = tCap)
densCap + geom_vline(aes(xintercept=ret$Capacitance),   
                     color="red", linetype="dashed", size=1)

# Resistance
tRes <- paste("Resistance of Return: ", ret$Resistance)
hisRes <- ggplot(good, aes(good$Resistance)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 colour="black", fill="white")
densRes <- hisRes + geom_density(alpha=0.2, fill="#FF6666") +    # Overlay density
  theme(legend.position = "none") +
  labs(x = "Resistance", title = tRes)
densRes + geom_vline(aes(xintercept=ret$Resistance),   
                     color="red", linetype="dashed", size=1)


# VccCapacitance
tVCap <- paste("Powered Capacitance of Return: ", ret$VccCapacitance)
hisVCap <- ggplot(good, aes(good$VccCapacitance)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 colour="black", fill="white")
densVCap <- hisVCap + geom_density(alpha=0.2, fill="#FF6666") +    # Overlay density
  theme(legend.position = "none") +
  labs(x = "Vcc-Capacitance", title = tVCap)
densVCap + geom_vline(aes(xintercept=ret$VccCapacitance),   
                     color="red", linetype="dashed", size=1)

# VccResistance
tVRes <- paste("Powered Resistance of Return: ", ret$VccResistance)
hisVRes <- ggplot(good, aes(good$VccResistance)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 colour="black", fill="white")
densVRes <- hisVRes + geom_density(alpha=0.2, fill="#FF6666") +    # Overlay density
  theme(legend.position = "none") +
  labs(x = "Vcc-Resistance", title = tVRes)
densVRes + geom_vline(aes(xintercept=ret$VccResistance),   
                     color="red", linetype="dashed", size=1)

# P1Capacitance
tP1Cap <- paste("P1 Capacitance of Return: ", ret$P1Capacitance)
hisP1Cap <- ggplot(good, aes(good$P1Capacitance)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 colour="black", fill="white")
densP1Cap <- hisP1Cap + geom_density(alpha=0.2, fill="#FF6666") +    # Overlay density
  theme(legend.position = "none") +
  labs(x = "P1 Capacitance", title = tP1Cap)
densP1Cap + geom_vline(aes(xintercept=ret$P1Capacitance),   
                     color="red", linetype="dashed", size=1)

# P1Resistance
tP1Res <- paste("P1 Resistance of Return: ", ret$P1Resistance)
hisP1Res <- ggplot(good, aes(good$P1Resistance)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 colour="black", fill="white")
densP1Res <- hisP1Res + geom_density(alpha=0.2, fill="#FF6666") +    # Overlay density
  theme(legend.position = "none") +
  labs(x = "P1 Resistance", title = tP1Res)
densP1Res + geom_vline(aes(xintercept=ret$P1Resistance),   
                     color="red", linetype="dashed", size=1)

# P2Capacitance
tP2Cap <- paste("P2 Capacitance of Return: ", ret$P2Capacitance)
hisP2Cap <- ggplot(good, aes(good$P2Capacitance)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 colour="black", fill="white")
densP2Cap <- hisP2Cap + geom_density(alpha=0.2, fill="#FF6666") +    # Overlay density
  theme(legend.position = "none") +
  labs(x = "P2 Capacitance", title = tP2Cap)
densP2Cap + geom_vline(aes(xintercept=ret$P2Capacitance),   
                       color="red", linetype="dashed", size=1)

# P2Resistance
tP2Res <- paste("P2 Resistance of Return: ", ret$P2Resistance)
hisP2Res <- ggplot(good, aes(good$P2Resistance)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 colour="black", fill="white")
densP2Res <- hisP2Res + geom_density(alpha=0.2, fill="#FF6666") +    # Overlay density
  theme(legend.position = "none") +
  labs(x = "P2 Resistance", title = tP2Res)
densP2Res + geom_vline(aes(xintercept=ret$P2Resistance),   
                       color="red", linetype="dashed", size=1)

# Cap Delta
DelCap <- good$VccCapacitance - good$Capacitance
DelRes <- good$VccResistance - good$Resistance
DelCapRet <- ret$VccCapacitance - ret$Capacitance
DelResRet <- ret$VccResistance - ret$Resistance

# Delta Capacitance
tDelCap <- paste("Capacitance (Powered - Unpowered) of Return: ", DelCapRet)
hisDelCap <- ggplot(good, aes(DelCap)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 colour="black", fill="white")
densDelCap <- hisDelCap + geom_density(alpha=0.2, fill="#FF6666") +    # Overlay density
  theme(legend.position = "none") +
  labs(x = "Capacitance Delta", title = tDelCap)
densDelCap + geom_vline(aes(xintercept=DelCapRet),   
                     color="red", linetype="dashed", size=1)

# Resistance
tDelRes <- paste("Resistance (Powered - Unpowered) of Return: ", DelResRet)
hisDelRes <- ggplot(good, aes(DelRes)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 colour="black", fill="white")
densDelRes <- hisDelRes + geom_density(alpha=0.2, fill="#FF6666") +    # Overlay density
  theme(legend.position = "none") +
  labs(x = "Resistance Delta", title = tDelRes)
densDelRes + geom_vline(aes(xintercept=DelResRet),   
                        color="red", linetype="dashed", size=1)

