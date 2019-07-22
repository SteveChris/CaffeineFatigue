library(ggplot2)
library(data.table)

#The first thing I did was plot the biomath values over time (copy+paste from LISP easier the file output)

{#My test code to get these numbers is in run-model
  bmv = c(3.599371263362123 ,3.5479107248317323 ,4.413394230138752 ,6.408429444971388 ,9.031257653927538,12.09834383462202,14.71066879375729,16.527669348980577,17.17849670167077,16.74608427261302)
  hours = c(65.85513,67.84515,69.75257,71.81277,73.76906,75.85811,77.84787,79.90474,81.88346,83.88346)
  qplot(hours,bmv,xlab = "hour",ylab="biomath value",geom="line")
  qplot(1:length(hours),bmv,xlab = "bout",ylab="biomath value",geom=c("point","line"),main = "Biomath Value by bout") +
    scale_x_continuous(breaks=seq(0,9,3)) 
}


#So one would expect that empirical RT mean by hour would have the same shape as biomath
#especially with McCauley's comment
#"Schedule-based comparisons of predicted fatigue by time point and by time spent above some threshold level
# are nominally invariant to what measure of fatigue is considered, 
#provided a monotonic relationship between different fatigue measures may be assumed."
source("parseEmpirical.R") 
source("parseACT-RMulti.R")
#So what happens if we put them together?
{#wholeTable = rbind(subset(model_outputs,as.numeric(varVal)>=-.023),empirical, use.names=TRUE)
  model_outputs = model_outputs[var=="UTBMC" ]#| varVal <0.62]
  wholeTable = rbind(model_outputs,empirical, use.names=TRUE)
  
  pd = position_dodge(0.1)
  bothPlt = ggplot(data=wholeTable,aes(x=bout,y=dRTMean,group=paste(var,varVal),colour=paste(var,varVal))) +
    #  geom_errorbar(aes(ymin=dRTMean-dRTSD, ymax=dRTMean+dRTSD), width=.1,position = pd) +
    geom_point(position = pd) + 
    geom_line(position = pd) +
    scale_x_continuous(breaks=1:9) +
    ggtitle("ACT-R outputs compared to empirical data")
  
  bothPlt
}
#It looks pretty good now lmao

#If we want to switch it to hour of day instead of bout, it looks similar
{  bothPlt = ggplot(data=wholeTable,aes(x=63.85513+bout*2,y=dRTMean,group=var,colour=var)) +
    #  geom_errorbar(aes(ymin=dRTMean-dRTSD, ymax=dRTMean+dRTSD), width=.1,position = pd) +
    geom_point(position = pd) + 
    geom_line(position = pd) +
    scale_x_continuous(name="hour of day",breaks=hours,labels = round(hours%%24)) +
    ggtitle("ACT-R outputs compared to empirical data")
  
  bothPlt
}
#And if we want to use just alert RT (cleaner), then the result is kinda bad...
{  bothPlt = ggplot(data=wholeTable,aes(x=63.85513+bout*2,y=dRTAlert,group=var,colour=var)) +
    #  geom_errorbar(aes(ymin=dRTMean-dRTSD, ymax=dRTMean+dRTSD), width=.1,position = pd) +
    geom_point(position = pd) + 
    geom_line(position = pd) +
    scale_x_continuous(name="hour of day",breaks=hours,labels = round(hours%%24)) +
    ggtitle("ACT-R outputs compared to empirical data (Alert)")
  
  bothPlt
}
#A lot of the modeling efforts are based on lapses. Are we in shape for those?
{
  bothPlt = ggplot(data=wholeTable,aes(x=bout,y=dRTLapses,group=varVal,colour=varVal)) +
    #  geom_errorbar(aes(ymin=dRTMean-dRTSD, ymax=dRTMean+dRTSD), width=.1,position = pd) +
    geom_point(position = pd) + 
    geom_line(position = pd) +
    ggtitle("ACT-R outputs compared to empirical data (Lapse probability)")
  
  bothPlt
}
