library(tidyverse)
library(ggplot2)
library(trend)
library(ggExtra)
library(ggrepel)
library(scales)
library(readxl)
library(sf)
library(reshape2)
library(gridExtra)

getFullData = function(){
setwd("/Users/9oy/Documents/Projects/DSFResearch/Paper1/Data/Impervious/Impervious")
files = list.files(pattern = "*.xlsx")
length(files)
dfI = data.frame()

for (i in 1:length(files)){
  subdf = read_excel(files[i],.name_repair = "minimal")
  dfI = rbind(dfI,subdf)
}

setwd("/Users/9oy/Documents/Projects/DSFResearch/Paper1/Data/Greenery/TreeCoveragg")
files = list.files(pattern = "*.xlsx")
length(files)
dfTC = data.frame()

for (i in 1:length(files)){
  subdf = read_excel(files[i],.name_repair = "minimal")
  dfTC = rbind(dfTC,subdf)
}

colnames(dfI)[1] = "ID"
dfI = dfI
dfI[,3:9] = apply(dfI[,3:9],2,as.numeric)

colnames(dfTC)[1] = "ID"
dfTC = dfTC
dfTC[,3:9] = apply(dfTC[,3:9],2,as.numeric)
dfI$Type = "Developed Land"
dfTC$Type = "Greenery"

df = rbind(dfI,dfTC) 
#df30 = df %>% filter(Scale==30)
df$MaxH = sqrt(df$Mean *(1-df$Mean))
df$MCH = df$STD/df$MaxH
return(df)
}

getthdf =function(xmin,xmax){
  x = seq(xmin,xmax,0.001)
  y = sqrt(x * (1-x))
  thdf = data.frame(x,y)
  return(thdf)
}

getFullTractData = function(){
  setwd("/Users/9oy/Documents/Projects/DSFResearch/Paper1/Data/ImperviousTract/")
  files = list.files(pattern = "*.xlsx")
  length(files)
  dfI = data.frame()
  
  for (i in 1:length(files)){
    subdf = read_excel(files[i],.name_repair = "minimal")
    dfI = rbind(dfI,subdf)
  }
  
  setwd("/Users/9oy/Documents/Projects/DSFResearch/Paper1/Data/TreeCoveraggTract")
  files = list.files(pattern = "*.xlsx")
  length(files)
  dfTC = data.frame()
  
  for (i in 1:length(files)){
    subdf = read_excel(files[i],.name_repair = "minimal")
    dfTC = rbind(dfTC,subdf)
  }
  
  colnames(dfI)[1] = "ID"
  dfI = dfI
  dfI[,4:9] = apply(dfI[,4:9],2,as.numeric)
  
  colnames(dfTC)[1] = "ID"
  dfTC = dfTC
  dfTC[,4:9] = apply(dfTC[,4:9],2,as.numeric)
  dfI$Type = "Built Infrastructure"
  dfTC$Type = "Tree Cover"
  
  df = rbind(dfI,dfTC) 
  #df30 = df %>% filter(Scale==30)
  df$MaxH = sqrt(df$Mean *(1-df$Mean))
  df$MCH = df$STD/df$MaxH
  return(df)
}

getSTD = function(df,seed){
  set.seed(seed)
  mchse = (df[["MCHF2050_95_H"]] - df[["MCHF2050_95_L"]]) / (2 * 1.96)
  mch = rnorm(100, mean = df[["MCHF2050"]], sd = mchse)
  mch[mch < 0] = 0
  mch[mch > 1] = 1
  muse = (df[["MeanF2050_95_H"]] - df[["MeanF2050_95_L"]]) / (2 * 1.96)
  mu = rnorm(100, mean = df[["MeanF2050"]], sd = muse)
  mu[mu < 0] = 0
  mu[mu > 1] = 1
  std = mch * sqrt(mu * (1-mu))
  std[is.na(std)]= 0
  cistd <- as.numeric(quantile(std, probs = c(0.025, 0.975)))
  return(c(mean(std),cistd))
}
hetforecast = function(df30,type="Built Infrastructure",seed){
  allmetros = unique(df30$NAME)
  ## Forecast MCH
  outdfMCH = data.frame()
  for(i in 1:380){
    subdf= subset(df30,(df30$NAME==allmetros[i]) & (df30$Type==type) & (df30$Year %in% 1985:2018))
    modAR = auto.arima(ts(subdf$MCH,start=1985))
    pseudo_R2 = cor(fitted(modAR), ts(subdf$MCH,start=1985))^2
    F2023 = forecast(modAR,h = 5)[4]$mean[5]
    F2023[F2023 < 0] = 0
    F2023[F2023 > 1] = 1
    F2023_95_L = as.numeric(forecast(modAR,h = 5)[5]$lower[5,2])
    F2023_95_L[F2023_95_L < 0] = 0
    F2023_95_L[F2023_95_L > 1] = 1
    F2023_95_H = as.numeric(forecast(modAR,h = 5)[6]$upper[5,2])
    F2023_95_H[F2023_95_H < 0] = 0
    F2023_95_H[F2023_95_H > 1] = 1
    A2023 = subset(df30,(df30$NAME==allmetros[i]) & (df30$Type==type) & (df30$Year %in% 2023))$MCH
    
    F2050 = forecast(modAR,h = 32)[4]$mean[32]
    F2050[F2050 < 0] = 0
    F2050[F2050 > 1] = 1
    F2050_95_L = as.numeric(forecast(modAR,h = 32)[5]$lower[32,2])
    F2050_95_L[F2050_95_L < 0] = 0
    F2050_95_L[F2050_95_L > 1] = 1
    F2050_95_H = as.numeric(forecast(modAR,h = 32)[6]$upper[32,2])
    F2050_95_H[F2050_95_H < 0] = 0
    F2050_95_H[F2050_95_H > 1] = 1
    suboutdf = data.frame(NAME=allmetros[i],F2023=F2023,F2023_95_L=F2023_95_L,
                          F2023_95_H=F2023_95_H,A2023=A2023,Type=type,R2=pseudo_R2,
                          F2050,F2050_95_L,F2050_95_H)
    outdfMCH = rbind(outdfMCH,suboutdf)
  }
  
  
  ## Forecast Mu
  outdMean = data.frame()
  for(i in 1:380){
    subdf= subset(df30,(df30$NAME==allmetros[i]) & (df30$Type==type) & (df30$Year %in% 1985:2018))
    modAR = auto.arima(ts(subdf$Mean,start=1985))
    pseudo_R2 = cor(fitted(modAR), ts(subdf$Mean,start=1985))^2
    F2023 = forecast(modAR,h = 5)[4]$mean[5]
    F2023[F2023 < 0] = 0
    F2023[F2023 > 1] = 1
    F2023_95_L = as.numeric(forecast(modAR,h = 5)[5]$lower[5,2])
    F2023_95_L[F2023_95_L < 0] = 0
    F2023_95_L[F2023_95_L > 1] = 1
    F2023_95_H = as.numeric(forecast(modAR,h = 5)[6]$upper[5,2])
    F2023_95_H[F2023_95_H < 0] = 0
    F2023_95_H[F2023_95_H > 1] = 1
    
    A2023 = subset(df30,(df30$NAME==allmetros[i]) & (df30$Type==type) & (df30$Year %in% 2023))$Mean
    
    F2050 = forecast(modAR,h = 32)[4]$mean[32]
    F2050_95_L[F2050_95_L < 0] = 0
    F2050_95_L[F2050_95_L > 1] = 1
    F2050_95_L = as.numeric(forecast(modAR,h = 32)[5]$lower[32,2])
    F2050_95_L[F2050_95_L < 0] = 0
    F2050_95_L[F2050_95_L > 1] = 1
    F2050_95_H = as.numeric(forecast(modAR,h = 32)[6]$upper[32,2])
    F2050_95_H[F2050_95_H < 0] = 0
    F2050_95_H[F2050_95_H > 1] = 1
    
    suboutdf = data.frame(NAME=allmetros[i],F2023=F2023,F2023_95_L=F2023_95_L,
                          F2023_95_H=F2023_95_H,A2023=A2023,Type=type,R2=pseudo_R2,
                          F2050,F2050_95_L,F2050_95_H)
    outdMean = rbind(outdMean,suboutdf)
  }
  ## Forecast Heterogeneity
  
  FMCH = outdfMCH[,c("NAME","F2050","F2050_95_L","F2050_95_H")]
  colnames(FMCH) = paste("MCH",colnames(FMCH),sep="")
  FMean = outdMean[,c("NAME","F2050","F2050_95_L","F2050_95_H")]
  colnames(FMean) = paste("Mean",colnames(FMean),sep="")
  
  FDF = merge(FMCH,FMean,by.x="MCHNAME",by.y="MeanNAME")
  Fordf = data.frame()
  for (i in 1:380){
    out = getSTD(FDF[i,],seed)
    out = data.frame(NAME = FDF[i,"MCHNAME"],STD2050=out[1],STD2050L=out[2],STD2050H = out[3])
    Fordf = rbind(Fordf,out)
  }
  
  ADF2023 = df30%>% filter(Type==type) %>% filter(Year==2023)
  
  FDF = merge(ADF2023,Fordf,by="NAME")
  return(list(FDF,outdfMCH,outdMean))
}