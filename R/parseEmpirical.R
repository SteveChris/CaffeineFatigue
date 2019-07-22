#make sure we're in the right directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#load data
PVTdf=fread('../Empirical/allPVT.csv', header = TRUE, sep = ",", strip.white = TRUE)#, nrows=17187)

#toAdd = PVTdf$bouttime<1200 #taking out stupid values/outliers
#I'm being dumb converting back and forth between hour types and bouttimes, etc
PVTdf$hour = ifelse(is.na(PVTdf$bouttime),82, 48 + PVTdf$bouttime/100 + ifelse(PVTdf$bouttime < 1200, 24, 0))

empirical = data.table()

for (cond in c("caffeine", "control")){
  part = subset(PVTdf,condition==cond)
  for (bout in unique(part$Bout)){
    boutRTs = part[Bout==bout,RT]
    alertRTs = boutRTs[boutRTs>150 & boutRTs<500]
    lapse_pct = length(boutRTs[boutRTs<150])/length(boutRTs)
    
    dRTMean = mean(boutRTs)
    tt = t.test(boutRTs)$conf.int
    RTL = tt[1]
    RTU = tt[2]
    
    dRTAlert = mean(alertRTs)
    tt = t.test(alertRTs)$conf.int
    alertRTL = tt[1]
    alertRTU = tt[2]
    
    var = paste("emp.", cond) 
    varVal = 0
    row = data.table(dRTMean, RTL, RTU, dRTAlert, alertRTL, alertRTU, lapse_pct, bout, var, varVal)
    empirical = rbind(empirical,row)
  }
}
ggplot(data=empirical,aes(x=bout,y=dRTMean,group=var,colour=var)) +
  geom_errorbar(aes(ymin=RTL, ymax=RTU), width=.1,position = pd) +
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks=1:9) +
  ggtitle("Empirical reaction time means")

