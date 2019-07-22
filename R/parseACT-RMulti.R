library(ggplot2)
library(data.table)
library(stringr)
library(fs)

#make sure we're in the right directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
targDir = "../actr75/PVT-outputs/"

#This is because we know what that hours are. Yes hardcoded
hour2bout <- function (hour) {
  h = round(hour,4)
  knownHours = round(c(65.8551,67.84515,69.75257,71.81277,73.76906,75.85811,77.84787,79.90474,81.88346),4)
  bout = match(h,knownHours)
  if(is.na(bout[1])){
    bout = match(h,(knownHours+2))
  }
  return(bout)
}

#we go through every file, taking summary measures, and saving them in a table
model_outputs = data.table()
varTypes = c("UTBMC", "EGS")
for (var in varTypes){
  targFiles = list.files(targDir,full.names = TRUE) #list.files does not work as documented, so we need to remove directory names
  targFiles = targFiles[!dir.exists(targFiles)]
  targFiles = str_subset(targFiles,var)
  #the table is indexed by bout and by variable value (in the example case we're looking at different values of UTBMC)
  for(f in targFiles){
    part = fread(f, header = TRUE, sep = ",", strip.white = TRUE,drop = c("fp","fp-percent","fpmc","utmc"))
    part = part[hour!="hour"] #There was a bug where the header line was repeated 
    part$RT = as.numeric(part$RT) #(caused by running multiple iterations on same value)
    
    #because in file it is day by hour instead of just hour or bout
    part$hour = as.numeric(part$hour)+as.numeric(part$day)*24 
    bHours = sort(unique(part$hour))
    for (b in bHours){#There is no 'bout' column, so I use hour here instead
      bout = hour2bout(b)
      boutRTs = part[hour==b,RT] #This line is important to change if you try to copy+paste between empirical and act-r
      dRTMean = mean(boutRTs)
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
      #varVal = substr(f,str_locate(f,var)[2]+1,str_length(f))#one-line way to get just the param value
      varVal = substr(f,str_locate(f,var)[2]+1,str_length(f))
      row = data.table(dRTMean, RTL, RTU, dRTAlert, alertRTL, alertRTU, lapse_pct, var, varVal,bout)
      model_outputs = rbind(model_outputs,row)
    }
  }
  
}
#TODO: remember that utbmc here as things are as of July 12 2019 is the part and all the egs are caff