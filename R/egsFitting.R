#before bout 6, there is no caffeine, so we just assume it stays at 0.46 as before

#This script ONLY makes sense to be called after running iterateEGS

library(ggplot2)
library(data.table)
library(rstudioapi)
#make sure we're in the right directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
targDir = "../actr75/PVT-outputs/"

#This chunk is ONLY for fitting egs the first 5 bouts of combined data (since, by McKinley, before bout 6, both groups are same)
#further, this chunk is deprecated because I don't use R2 for fitness anymore. Use the one below it
#The only reason I am keeping it is because Chris Myers suggested using R^2 to get a line fit rather than vague fit
{
  keep = c("subjects","min","egs","R2")
  f = paste0(targDir,"blocksummaryR2.txt")
  caffsums = fread(f, header = TRUE, sep = ",", strip.white = TRUE,select = keep)
  caffsums = caffsums[subjects!="subjects"] #drop duplicate header rows
  caffsums$R2 = as.numeric(caffsums$R2) #duplicate header rows also forced my data to strings >.<
  caffsums$min = as.numeric(caffsums$min)
  caffsums$egs = as.numeric(caffsums$egs)
  
  fitness = c() #this is my super duper important RMSE / R^2 composite thing
  for(i in sort((unique(caffsums$egs)))){ #TODO: don't use a map function b/c I want people to understand my code
    fitness = c(fitness, sum(caffsums[egs==i & min <5,R2]))#remember min is proxy for bout
  }
  qplot(sort(unique(caffsums$egs)),fitness) + scale_x_continuous()
  #and the highest point on here is the EGS that we want to use for out first 5 bouts
}
#Use this chunk to get the best egs
{
  keep = c("subjects","min","egs","rmse")
  f = paste0(targDir,"blocksummaryRMSE.txt")
  caffsums = fread(f, header = TRUE, sep = ",", strip.white = TRUE,select = keep)
  caffsums = caffsums[subjects!="subjects"] #drop duplicate header rows
  caffsums$rmse = as.numeric(caffsums$rmse) #duplicate header rows also forced my data to strings >.<
  caffsums$min = as.numeric(caffsums$min)
  
  fitness = c() #this is my super duper important RMSE / R^2 composite thing
  for(i in sort((unique(caffsums$egs)))){ #TODO: don't use a map function b/c I want people to understand my code
    fitness = c(fitness, 1/sum(caffsums[egs==i & min <5,rmse]))#remember min is proxy for bout
    #Optimal fitness is the same whether 1/sum or negative sum
    }
  plot(sort(unique(caffsums$egs)),fitness)
  #and the HIGHEST point here is the EGS that we want to use for our first 5 bouts
  bestEGS = sort(unique(caffsums$egs))[which.max(fitness)]
  print(bestEGS)
}


groups = c("_caff","_decaf")
keep = c("subjects","min","egs","rmse")

bests = data.table()
for(g in groups){
  f = paste0(targDir,"blocksummary.txt",g)
  if(file.exists(f)){
    caffsums = fread(f, header = TRUE, sep = ",", strip.white = TRUE,select = keep)
    caffsums = caffsums[subjects!="subjects"] #drop duplicate header rows
    caffsums$rmse = as.numeric(caffsums$rmse) #duplicate header rows also forced my data to strings >.<
    caffsums$egs = as.numeric(caffsums$egs) #duplicate header rows also forced my data to strings >.<
    
    #read through each bout, picking the EGS that gives the best fit value
    for(b in unique(caffsums$min)){#min is equivalent to bout because I poached code and didn't change it for backwards compatibility reasons
      bout = as.numeric(b)+1
      disBout = caffsums[min==b]
      
      ind = which.min(disBout$rmse)
      bestRMSE = disBout[ind]$rmse
      bestEGS = disBout[ind]$egs
      row = data.table(bout, bestRMSE, bestEGS, g)
      bests = rbind(bests,row)
    }
  } else {
    print(paste(f,"does not exist! \n and you should feel bad"))
  }
}

  
#Now that we all have the EGSs and the bout, we can define EGS as a function of bout!
pd = position_dodge(0.1)
bothPlt = ggplot(data=bests,aes(x=bout,y=bestEGS,group=g,colour=g)) +
  geom_point(position = pd) + 
  geom_line(position = pd) +
  scale_x_continuous(breaks=1:9) +
  ggtitle("Best-fit EGSs as a function of bout")

bothPlt


#This *may* make a pretty graph, but it's not important in itself
#because caffeine is also a function of bout
#we can define EGS as a function of caffeine

#So here's a graph of caffeine as a function of bout
caffConcs = fread("../PBPK outputs/caffModelOuts.txt")
names(caffConcs) = c("t","concentration")
#caffeine is administered at 0315 (but because trials tended to happen early and because gum but PBPK is instant coffee, we will say 0300), 
#which is betwixt bouts 5 and 6
caffConcs$t = caffConcs$t/2 + 5.5
caffConcs = caffConcs[t<=9]
#TODO: May need to add reference values in here before use, but this works for the graph
zeroMesh = seq(1,5.5,.1)
caffConcs = rbind(caffConcs, data.table(zeroMesh,rep(0,length(zeroMesh))), use.names=FALSE)

#now we just need to do egs as a function of caffeine
tMesh = seq(1,9,.1)
PBPKPlt = ggplot(data=caffConcs,aes(x=t,y=concentration)) +
  geom_line()

PBPKPlt
