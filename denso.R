######################################################################
#                                                                    #
#                        Basic Conditioning                          #
######################################################################

denso730 <- read.csv('DENSOC40670992MSG.csv')

# calib <- read.csv('denso-calib-731.csv') not good b/c cannot order time seq well ...
calib <- read.csv('cal-10pp1110-0810.csv') # time saved as numeric yay!


# read library into working environment
library(ggplot2)

######################################################################
#                              Density                               #
######################################################################
# try the density plot on raw input
# span err
ggplot(calib, aes(P1SpanErr)) + geom_density(aes(fill=factor(DeviceID), alpha=0.2), size=1) +
  xlim(0,0.7) + facet_grid(Mode ~ .) + labs(x = "P1 Span Error")
# loP1 err
ggplot(calib, aes(calib$EEPROMLoP1Err)) + geom_density(aes(fill=factor(DeviceID), alpha=0.2), size=1) +
  xlim(-1,1) + facet_grid(Mode ~ .) + labs(x = "0Bar P1 Error")
# hiP1 err
ggplot(calib, aes(calib$EEPROMHiP1Err)) + geom_density(aes(fill=factor(DeviceID), alpha=0.2), size=1) +
  xlim(-1,1) + facet_grid(Mode ~ .) + labs(x = "~220Bar P1 Error")



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

# high press
leakbx <-  ggplot(calib, aes(factor(DeviceID), calib$HiPressLeakRate))
leakbx + geom_boxplot(outlier.colour = "red")  + facet_grid(. ~ Mode) + 
  aes(fill = factor(DeviceID)) + theme(legend.position="none")

# lo press
loleakbx <-  ggplot(calib, aes(factor(DeviceID), calib$FinalLoPressLeakRate))
loleakbx + geom_boxplot(outlier.colour = "red") + ylim(0,0.25) + facet_grid(. ~ Mode) + 
  aes(fill = factor(DeviceID)) + theme(legend.position="none")

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


del1bx <- ggplot(t123aug, aes(factor(DeviceID), delLoPT1)) + geom_boxplot(outlier.colour = "red")
del1 <- del1bx + aes(fill = factor(DeviceID)) + theme(legend.position="none") + ylim(-0.0325,0.0125)
del1

deladc1bx <- ggplot(t123aug, aes(factor(DeviceID), delLoPadcT1)) + geom_boxplot(outlier.colour = "red")
deladc1 <- deladc1bx + aes(fill = factor(DeviceID)) + theme(legend.position="none") + ylim(-150,35)
deladc1

del2bx <- ggplot(t123aug, aes(factor(DeviceID), delLoPT2)) + geom_boxplot(outlier.colour = "red")
del2 <- del2bx + aes(fill = factor(DeviceID)) + theme(legend.position="none") + ylim(-0.0325,0.0125)
del2

deladc2bx <- ggplot(t123aug, aes(factor(DeviceID), delLoPadcT2)) + geom_boxplot(outlier.colour = "red")
deladc2 <- deladc2bx + aes(fill = factor(DeviceID)) + theme(legend.position="none") + ylim(-150,35)
deladc2

del3bx <- ggplot(t123aug, aes(factor(DeviceID), delLoPT3)) + geom_boxplot(outlier.colour = "red")
del3 <- del3bx + aes(fill = factor(DeviceID)) + theme(legend.position="none") + ylim(-0.0325,0.0125)
del3

deladc3bx <- ggplot(t123aug, aes(factor(DeviceID), delLoPadcT3)) + geom_boxplot(outlier.colour = "red")
deladc3 <- deladc3bx + aes(fill = factor(DeviceID)) + theme(legend.position="none") + ylim(-150,35)
deladc3

# *** IMPORTANT PLOT *** #
multiplot(del1, deladc1, del2, deladc2, del3, deladc3, cols=3)

# want to plot lopress error against some digital gain terms
# T3
T3part <- c(1:length(calT3filt$TestTime))
loP1T3 <- ggplot(calT3filt,aes(T3part)) +
  geom_point(aes(y = calT3filt$EEPROMLoP1Err, color = factor(calT3filt$DeviceID)))
loP1T3
loP1T3 + ylim(-0.5,0.5)

hiP1T3 <- ggplot(calT3filt,aes(T3part)) +
  geom_point(aes(y = calT3filt$EEPROMHiP1Err, color = factor(calT3filt$DeviceID)))
hiP1T3
hiP1T3 + ylim(-0.5,0.5)

# T2
T2part <- c(1:length(calT2filt$TestTime))
loP1T2 <- ggplot(calT2filt,aes(T2part)) +
  geom_point(aes(y = calT2filt$EEPROMLoP1Err, color = factor(calT2filt$DeviceID)))
loP1T2
loP1T2 + ylim(-0.5,0.5)

# T1
T1part <- c(1:length(calT1filt$TestTime))
loP1T1 <- ggplot(calT1filt,aes(T1part)) +
  geom_point(aes(y = calT1filt$EEPROMLoP1Err, color = factor(calT1filt$DeviceID)))
loP1T1
loP1T1 + ylim(-0.5,0.5)



allpart <- c(1:length(t123$SerialNo))

# lo P errors
LoP1Tall <- ggplot(t123, aes(allpart)) + 
  geom_point(aes(y = t123$EEPROMLoP1Err.t1, shape ="t1",color = t123$DeviceID.t1)) + 
  geom_point(aes(y = t123$EEPROMLoP1Err.t2, shape = "t2",color = t123$DeviceID.t2)) +
  geom_point(aes(y = t123$EEPROMLoP1Err, shape = "t3",color = t123$DeviceID))
LoP1Tall + ylim(-0.5,0.5)

# Hi P errors
HiP1Tall <- ggplot(t123, aes(allpart)) + 
  geom_point(aes(y = t123$EEPROMHiP1Err.t1, shape ="t1",color = t123$DeviceID.t1)) + 
  geom_point(aes(y = t123$EEPROMHiP1Err.t2, shape = "t2",color = t123$DeviceID.t2)) +
  geom_point(aes(y = t123$EEPROMHiP1Err, shape = "t3",color = t123$DeviceID))
HiP1Tall + ylim(-0.5,0.5)

# Span P errors
SpanP1Tall <- ggplot(t123, aes(allpart)) + 
  geom_point(aes(y = t123$P1SpanErr.t1, shape ="t1",color = t123$DeviceID.t1)) + 
  geom_point(aes(y = t123$P1SpanErr.t2, shape = "t2",color = t123$DeviceID.t2)) +
  geom_point(aes(y = t123$P1SpanErr, shape = "t3",color = t123$DeviceID))
SpanP1Tall + ylim(-0.5,0.5)

# T3 correlation between LoP1Err and input MVA
corLoPMva <- ggplot(t123, aes(t123$EEPROMLoP1Err)) + 
  geom_point(aes(y = t123$InitialLoPressADCP, shape ="t3"))
corLoPMva
corLoPMvatrim <- corLoPMva + xlim(-0.5,0.5) + ylim(-9000,-7500) + aes(color=t123$DeviceID)
corLoPMvatrim

# T1 correlation betw LoP1Err and input MVA
corLoPMvat1 <- ggplot(t123, aes(t123$EEPROMLoP1Err.t1)) + 
  geom_point(aes(y = t123$InitialLoPressADCP.t1))
corLoPMvat1 + xlim(-0.5,0.5) + ylim(-9000,-7500) + aes(color=t123$DeviceID)

# T2 correlation betw LoP1Err and input MVA
corLoPMvat2 <- ggplot(t123, aes(t123$EEPROMLoP1Err.t2)) + 
  geom_point(aes(y = t123$InitialLoPressADCP.t2))
corLoPMvat2 + xlim(-0.5,0.5) + ylim(-9000,-7500) + aes(color=t123$DeviceID)

#
calT3filtasc <- calT3asc %>% distinct(SerialNo, .keep_all = TRUE)
calT2filtasc <- calT2asc %>% distinct(SerialNo, .keep_all = TRUE)
calT1filtasc <- calT1asc %>% distinct(SerialNo, .keep_all = TRUE)




rownames(calT3filt) <- calT3filt$SerialNo

t1err <- calT1filt[,c("SerialNo","P1SpanErr")]
t2err <- calT2filt[,c("SerialNo","P1SpanErr")]
t3err <- calT3filt[,c("SerialNo","P1SpanErr")]

t12err <- right_join(t1err, t2err, by = c("SerialNo"), suffix = c(".t1", ".t2"))
t123err <- inner_join(t12err, t3err, by = c("SerialNo"), suffix = c("", ".t3"))
P1error_sort <- t123err[rev(order(t123err[,c("P1SpanErr")])),]

# do line plots
ggplot(P1error_sort, aes(P1error_sort$P1SpanErr.t1)) + 
  geom_line(aes(y = P1SpanErr.t2)) + 
  geom_line(aes(y = P1SpanErr))
#color dot version

# this is the span error
ggplot(P1error_sort, aes(P1SpanErr.t1)) + 
  geom_point(aes(y = P1SpanErr.t2, colour = "P1SpanErr.t2")) + 
  geom_point(aes(y = P1SpanErr, colour = "P1SpanErr"))



part <- c(1:length(P1error_sort$SerialNo))
P1error_sort <- data.frame(part, P1error_sort)

ggplot(P1error_sort, aes(part)) + 
  geom_point(aes(y = P1SpanErr.t1, colour = "P1SpanErr.t1")) + 
  geom_point(aes(y = P1SpanErr.t2, colour = "P1SpanErr.t2")) +
  geom_point(aes(y = P1SpanErr, colour = "P1SpanErr"))

# 