library(plyr)
library(ggplot2)
library(lubridate)
library(reshape2)

#Reading RDS files 
#NOTE: Change directory for your use
directory[1] <- "C:/Users/starkmanm/Documents/Dispersion/parthist_processed/"
filenames1 <- list.files(directory[1], pattern = ".rds", full.names = TRUE,recursive = T)

b1 <- llply(filenames1,.progress = "text", .fun = function(x){
  site <- basename(dirname(x))
  outdir=paste0("Scenarios")
  outname=basename(tools::file_path_sans_ext(x))
  a1=readRDS(x) 
  a1$fileDT_LT <- with_tz(a1$fileDT1,tz = "America/Los_Angeles") #PST
  a1$issueDT_LT <- with_tz(a1$issueDT1,tz = "America/Los_Angeles") #PST
  a1$particleDT_LT <- with_tz(a1$particleDT1,tz = "America/Los_Angeles") #PST
  scenarios<-data.frame(hours=seq(0,23),weight1=c(rep(1,24)),weight2=c(rep(0,8),rep(1,12),rep(0,4)),weight3=c(rep(0,8),rep(0.5,12),rep(0,4)))
  
  a1$hours <- hour(a1$fileDT_LT)
  a1 <-merge(a1,scenarios,by ="hours")
  
  #counts over the puff lifetime (more counts mean puff took longer to disperse)
  e <- ddply(a1,c("fileDT_LT","issueDT_LT","age"),function(x){
    w1_sum=sum(x$weight1,na.rm=T)
    w2_sum=sum(x$weight2,na.rm=T)
    w3_sum=sum(x$weight3,na.rm=T)
    Count=nrow(x)
    AvgPartHt = mean(x$Height)
    return(data.frame(w1_sum,w2_sum,w3_sum,Count,AvgPartHt))
  })
  e$site <- site
  
  #particle counts at a given time
  e1 <- ddply(a1,c("particleDT_LT","issueDT_LT","age"),function(x){
    w1_sum=sum(x$weight1,na.rm=T)
    w2_sum=sum(x$weight2,na.rm=T)
    w3_sum=sum(x$weight3,na.rm=T)
    Count=nrow(x)
    AvgPartHt = mean(x$Height)
    return(data.frame(w1_sum,w2_sum,w3_sum,Count,AvgPartHt))
  })
  e1$site <- site
  
  e2 <- ddply(a1,c("issueDT_LT","fileDT_LT","age"),function(x){
    w1_sum=sum(x$weight1,na.rm=T)
    w2_sum=sum(x$weight2,na.rm=T)
    w3_sum=sum(x$weight3,na.rm=T)
    Count=nrow(x)
    AvgPartHt = mean(x$Height)
    return(data.frame(w1_sum,w2_sum,w3_sum,Count,AvgPartHt))
  })
  units(e2$age) <- "hours"
  e2$site <- site
  
  return(list(e,e1,e2))
  
})
scenariosM=melt(scenarios,id.vars = c("hours"),variable.name = "Scenario",value.name="value")

PuffLife1 <- ldply(b1,function(x){return(x[[1]])})
PuffLife1$hours <- PuffLife1$fileDT_LT - PuffLife1$issueDT_LT
units(PuffLife1$hours)<- "hours"
units(PuffLife1$age)<- "hours"
PuffLife1$day<-factor(PuffLife1$hours<=24, levels = c("TRUE","FALSE"),labels = as.character(c("day1","day2")))
names(PuffLife1)[4]<-"weight1"
names(PuffLife1)[5]<-"weight2"
names(PuffLife1)[6]<-"weight3"

PuffLife<-melt(PuffLife1,c("fileDT_LT","issueDT_LT","age","Count","site","day","hours","AvgPartHt"),variable.name = "Scenario",value.name="ParticleCount")
PuffLife<- merge(PuffLife,scenariosM,by=c("Scenario","hours"))
tmp=arrange(PuffLife,Scenario,fileDT_LT,issueDT_LT,site,age)
PL_LR<- ddply(tmp,c("issueDT_LT","fileDT_LT","site"),function(x){
  x$LossRFrac<-diff(c(x$value[1]*x$ParticleCount[1]/x$ParticleCount[1],x$ParticleCount/x$ParticleCount))
  x$LossR<-diff(c(x$value[1]*x$ParticleCount[1],x$ParticleCount))
  return(x)
  })

TotalCount1 <-ldply(b1,function(x){return(x[[2]])})
TotalCount <- ddply(TotalCount1,c("issueDT_LT","particleDT_LT","site","age"),function(x){
  wt1=sum(x$w1_sum,na.rm = T)
  wt2=sum(x$w2_sum,na.rm = T)
  wt3=sum(x$w3_sum,na.rm = T)
  return(data.frame(wt1,wt2,wt3))
})
TotalCount$hours <- TotalCount$particleDT_LT - TotalCount$issueDT_LT
units(TotalCount$hours)<- "hours"
units(TotalCount$age)<- "hours"
TotalCount$day<-factor(TotalCount$hours<=24, levels = c("TRUE","FALSE"),labels = as.character(c("day1","day2")))
#min,max, and mean by groups of issueDT,site, and day
TC_L=melt(TotalCount,id.vars = c("particleDT_LT","issueDT_LT","site","age","hours","day"),variable.name = "Scenario",value.name="ParticleCount")

#Calculate max, min, and mean values of particle count
TC_Num1 <- ddply(TC_L,c("issueDT_LT","site","Scenario"),function(x){
  max =max(x$ParticleCount,na.rm=T)
  min =min(x$ParticleCount,na.rm=T)
  mean =mean(x$ParticleCount,na.rm=T)
  return(data.frame(max,min,mean))
})
TC_Num =melt(TC_Num1,id.vars = c("issueDT_LT","site","Scenario"),variable.name = "ScenarioMetric",value.name="Count")

nHours1 <- ldply(b1,function(x){return(x[[3]])})
nHours <- ddply(nHours1,c("fileDT_LT","issueDT_LT","site"),function(x){
  wt1=sum(x$w1_sum,na.rm = T)
  wt2=sum(x$w2_sum,na.rm = T)
  wt3=sum(x$w3_sum,na.rm = T)
  Count = nrow(x)
  return(data.frame(wt1,wt2,wt3,Count))
})
nHours$hours <- nHours$fileDT_LT - nHours$issueDT_LT
units(nHours$hours)<- "hours"
nHours$day<-factor(nHours$hours<=24, levels = c("TRUE","FALSE"),labels = as.character(c("day1","day2")))

dir.create(outdir,showWarnings = F,recursive = TRUE)
write.csv(TC_L, file = paste0(outdir,"/","TotalCount",".csv"),row.names = F)
write.csv(PuffLife, file = paste0(outdir,"/","PuffLife",".csv"),row.names = F)
write.csv(PuffLife, file = paste0(outdir,"/","nHours",".csv"),row.names = F)

