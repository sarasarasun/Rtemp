######################################################################
#                                                                    #
#                        Basic Conditioning                          #
######################################################################

#denso730 <- read.csv('DENSOC40670992MSG.csv')

# calib <- read.csv('denso-calib-731.csv') not good b/c cannot order time seq well ...
calib <- read.csv('fcaCALcontrol.csv') # time saved as numeric yay!


# read library into working environment
library(ggplot2)

######################################################################
#                              Density                               #
######################################################################
# try the density plot on raw input
# span err
ggplot(calib, aes(P1SpanErr)) + geom_density(aes(fill=factor(DeviceID), alpha=0.2), size=1) +
  xlim(0,0.7) + facet_grid(Mode ~ .) + labs(x = "P1 Span Error", title = "10PP12-01")
# loP1 err
ggplot(calib, aes(calib$EEPROMLoP1Err)) + geom_density(aes(fill=factor(DeviceID), alpha=0.2), size=1) +
  xlim(-1,1) + facet_grid(Mode ~ .) + labs(x = "0Bar P1 Error", title = "10PP12-01")
# hiP1 err
ggplot(calib, aes(calib$EEPROMHiP1Err)) + geom_density(aes(fill=factor(DeviceID), alpha=0.2), size=1) +
  xlim(-1,1) + facet_grid(Mode ~ .) + labs(x = "~220Bar P1 Error", title = "10PP12-01")

######################################################################
#                            Boxplot                                 #
######################################################################
# try some box plots
calib.bx <- ggplot(calib, aes(factor(DeviceID), P1SpanErr))
calib.bx + geom_boxplot() # raw
calib.bx + geom_boxplot() + ylim(0,1) # remove some extreme outliers...
calib.bx + geom_boxplot(outlier.colour = "red") + ylim(0,1) + facet_grid(. ~ Mode)
calib.bx + geom_boxplot(outlier.colour = "red") + ylim(0,1) + facet_grid(. ~ Mode) + 
  aes(fill = factor(DeviceID))

######################################################################
#               cleansing data for neater analysis                   #
######################################################################
# separate modes
calT1 <- calib[grep("T1Cal",calib$Mode,ignore.case = T,),]
calT2 <- calib[grep("T2Cal",calib$Mode,ignore.case = T,),]
calT3 <- calib[grep("T3Cal",calib$Mode,ignore.case = T,),]
# sort by reverse time order
class(calT3$TestTime) # convert to a time class first if not a time yet

# calT1$TestTime <- as.POSIXlt(as.character(calT1$TestTime), format="%m/%d/%Y %H:%M:%S")
# calT2$TestTime <- as.POSIXlt(as.character(calT2$TestTime), format="%m/%d/%Y %H:%M:%S")
# calT3$TestTime <- as.POSIXlt(as.character(calT3$TestTime), format="%m/%d/%Y %H:%M:%S")

# note it still won't work b/c dplyr distinct will not work with POSIXlt
class(calT3$TestTime) # confirm
# calT3copy <- calT3

# sort by time reverse order
# but there's still duplicates (reworked pieces)
calT1 <- calT1[rev(order(calT1[,c("TestTime")])),]
calT2 <- calT2[rev(order(calT2[,c("TestTime")])),]
calT3 <- calT3[rev(order(calT3[,c("TestTime")])),]

calT1asc <- calT1[(order(calT1[,c("TestTime")])),]
calT2asc <- calT2[(order(calT2[,c("TestTime")])),]
calT3asc <- calT3[(order(calT3[,c("TestTime")])),]


# remove duplicate re-work entries that are not the last
library(dplyr)

# calTxfilt is the reverse-ordered, SN-distinct time series
calT3filt <- calT3 %>% distinct(SerialNo, .keep_all = TRUE)
calT2filt <- calT2 %>% distinct(SerialNo, .keep_all = TRUE)
calT1filt <- calT1 %>% distinct(SerialNo, .keep_all = TRUE)

# join 3 cal tables
t12 <- right_join(calT1filt, calT2filt, by = c("SerialNo"), suffix = c(".t1", ".t2"))
t123 <- inner_join(t12, calT3filt, by = c("SerialNo"), suffix = c("", ".t3"))

# calculate adc slopes for each T
# should i choose initial lopress or finallopress?
# P or M?
# T1 ADC slope, LoPress diff
slpADCT1 <- (t123$HiPressADCP.t1 - t123$InitialLoPressADCP.t1) / (t123$HiPress.t1 - t123$InitialLoPress.t1)
delLoPT1 <- t123$FinalLoPress.t1 - t123$InitialLoPress.t1
delLoPadcT1 <- t123$FinalLoPressADCP.t1 - t123$InitialLoPressADCP.t1
# T2 ADC slope
slpADCT2 <- (t123$HiPressADCP.t2 - t123$InitialLoPressADCP.t2) / (t123$HiPress.t2 - t123$InitialLoPress.t2)
delLoPT2 <- t123$FinalLoPress.t2 - t123$InitialLoPress.t2
delLoPadcT2 <- t123$FinalLoPressADCP.t2 - t123$InitialLoPressADCP.t2
# T3 ADC slope
slpADCT3 <- (t123$HiPressADCP - t123$InitialLoPressADCP) / (t123$HiPress - t123$InitialLoPress)
delLoPT3 <- t123$FinalLoPress - t123$InitialLoPress
delLoPadcT3 <- t123$FinalLoPressADCP - t123$InitialLoPressADCP
t123aug <- data.frame(t123,slpADCT1,slpADCT2,slpADCT3, delLoPT1,delLoPT2,delLoPT3)
t123aug <- data.frame(t123aug, delLoPadcT1, delLoPadcT2, delLoPadcT3)

adc1bx <- ggplot(t123aug, aes(factor(DeviceID), slpADCT1)) + geom_boxplot(outlier.colour = "red")
adc1 <- adc1bx + aes(fill = factor(DeviceID)) + ylim(52,62) 

adc2bx <- ggplot(t123aug, aes(factor(DeviceID), slpADCT2)) + geom_boxplot(outlier.colour = "red")
adc2 <- adc2bx + aes(fill = factor(DeviceID)) + ylim(52,62)

adc3bx <- ggplot(t123aug, aes(factor(DeviceID), slpADCT3)) + geom_boxplot(outlier.colour = "red")
adc3 <- adc3bx + aes(fill = factor(DeviceID)) + ylim(52,62)

multiplot(adc1,adc2,adc3,cols=3)


del1bx <- ggplot(t123aug, aes(factor(DeviceID), t123aug$delLoPT1)) + geom_boxplot(outlier.colour = "red")
del1bx
del1 <- del1bx + aes(fill = factor(DeviceID)) + theme(legend.position="none") #+ ylim(-0.0325,0.0125)
del1

deladc1bx <- ggplot(t123aug, aes(factor(DeviceID), delLoPadcT1)) + geom_boxplot(outlier.colour = "red")
deladc1 <- deladc1bx + aes(fill = factor(DeviceID)) + theme(legend.position="none") #+ ylim(-150,35)
deladc1

del2bx <- ggplot(t123aug, aes(factor(DeviceID), delLoPT2)) + geom_boxplot(outlier.colour = "red")
del2 <- del2bx + aes(fill = factor(DeviceID)) + theme(legend.position="none") #+ ylim(-0.0325,0.0125)
del2

deladc2bx <- ggplot(t123aug, aes(factor(DeviceID), delLoPadcT2)) + geom_boxplot(outlier.colour = "red")
deladc2 <- deladc2bx + aes(fill = factor(DeviceID)) + theme(legend.position="none") #+ ylim(-150,35)
deladc2

del3bx <- ggplot(t123aug, aes(factor(DeviceID), delLoPT3)) + geom_boxplot(outlier.colour = "red")
del3 <- del3bx + aes(fill = factor(DeviceID)) + theme(legend.position="none") #+ ylim(-0.0325,0.0125)
del3

deladc3bx <- ggplot(t123aug, aes(factor(DeviceID), delLoPadcT3)) + geom_boxplot(outlier.colour = "red")
deladc3 <- deladc3bx + aes(fill = factor(DeviceID)) + theme(legend.position="none") #+ ylim(-150,35)
deladc3

# *** IMPORTANT PLOT *** #
multiplot(del1, deladc1, del2, deladc2, del3, deladc3, cols=3)