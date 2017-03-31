library(plyr)
library(ggplot2)

#reading files and storing them in "parthist" file
#NOTE: Change directory for your use
directory <- "C:/Users/starkmanm/Documents/Dispersion/Data"
filenames <- list.files(directory,pattern = "*.parthist",full.names = TRUE)

#filelist <- l_ply(filenames,.progress = "text", function(x){
  a = read.csv(x,sep="|",stringsAsFactors=F)
  site <- basename(strsplit(x,split ="_") [[1]][1])
  b=substr(a$X.latN.lonW.height.,2,(nchar(a$X.latN.lonW.height.)-1)) #remove first and last text element to remove brackets
  a$latN=as.numeric(sapply(b,FUN=function(x){strsplit(x,split=",")[[1]][1]}))
  a$lonW=as.numeric(sapply(b,FUN=function(x){strsplit(x,split=",")[[1]][2]}))
  a$Height=as.numeric(sapply(b,FUN=function(x){strsplit(x,split=",")[[1]][3]}))
  a$row <-rownames(a)
  #remove column
  a$X.latN.lonW.height.= NULL
  
  #turning all illegitimate values to NA
  index<- a$lonW == -1359.0
  a[index,"lonW"] <- NA
  index <- a$latN == -999
  a[index,"latN"] <- NA
  index <- a$Height == -996
  a[index,"Height"] <- NA
  
  #categorizing height below and above 500m
  a$Height_cat=ifelse(a$Height>100,yes = "Above","Below")
  #removing all NA values (dispersed)
  b=a[!is.na(a$Height),]
  
  #turning format from sheet (yearmonthdayhour) into organized time useful in R
  b$particleDT1 <- as.POSIXct(as.character(b$particleDT),"%Y%m%d%H",tz = "UTC")
  b$issueDT1 <- as.POSIXct(as.character(b$issueDT) ,"%Y%m%d%H",tz="UTC")
  b$fileDT1 <- as.POSIXct(as.character(b$fileDT) ,"%Y%m%d%H", tz="UTC")
  
  #determine particle age add to b
  b$age <- (b$particleDT1 - b$fileDT1)
  units(b$age)<- "hours"
  b1<- subset(b,b$age == 0)
  b1$site <- site
  
  outdir=paste0("parthist","/",site)
  outdir1=paste0("parthist")
  outname=basename(tools::file_path_sans_ext(x))
  
  dir.create(outdir,showWarnings = F,recursive = TRUE)
  #tools: 
  #saveRDS(b, file = paste0(outdir,"/",outname,".rds"))
  write.csv(b1, file = paste0(outdir1,"/","Height_Row",".csv"),row.names = F)
  return(b)
})