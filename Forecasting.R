rm(list=ls())
gc()
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
library(ggpmisc)
library(stargazer)
library(ggpmisc)
library(forecast)

source("/Users/9oy/Documents/Projects/DSFResearch/Paper1/Script/SuppFuncs.R")

df = getFullData()
df$Type[df$Type == "Developed Land"] = "Built Infrastructure"
df$Type[df$Type == "Greenery"] = "Tree Cover"
thdf = getthdf(0.27,0.33)
df30 = df %>% filter(Scale==30)

#### Forecasting Heterogeneity BI ##
outBI = hetforecast(df30,type="Built Infrastructure",seed=421)
FDF = outBI[[1]]
evalMCH = outBI[[2]]
evalMean = outBI[[3]]

mchcheck = ggplot(evalMCH, aes(x=A2023,y=F2023)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed", linewidth = 1) + 
  theme_bw() +   
  theme(axis.title = element_text(size=24),axis.text= element_text(size=14))+ 
  xlab(expression(Actual-MCH[2023])) +
  ylab(expression(Forecast-MCH[2023])) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")))

meancheck = ggplot(evalMean, aes(x=A2023,y=F2023)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed", linewidth = 1) + 
  theme_bw() +   
  theme(axis.title = element_text(size=24),axis.text= element_text(size=14))+ 
  xlab(expression(Actual-mu[2023])) +
  ylab(expression(Forecast-mu[2023])) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")))

withinrange = (FDF$STD >= FDF$STD2050L) & (FDF$STD <= FDF$STD2050H)

FDF$SStest = ifelse(withinrange,"Not Significant","Significant")
FDF$SS = ifelse(withinrange,
                paste("Not Significant:",as.character(table(FDF$SStest)[1]),sep=""),
                paste("Significant:",as.character(table(FDF$SStest)[2]),sep=" "))

BIplt = ggplot(FDF,aes(x=STD,y=STD2050,col=SS)) + geom_point() + geom_linerange(aes(ymin=STD2050L, ymax=STD2050H),alpha=0.4) + 
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed", linewidth = 1) + 
  theme_bw() +   theme(axis.title = element_text(size=24),
                       axis.text= element_text(size=14)) +
  labs(color = "95% Sig.",x=expression(sigma[2023]),y=expression(sigma[2050]),title="Built Infrastructure") +
  theme(legend.position = c(0.8, 0.2))

BIeval = gridExtra::grid.arrange(mchcheck,meancheck,ncol=2)

BIeval
BIplt

## Tree Cover Forecasts
outBI = hetforecast(df30,type="Tree Cover",seed=421)
FDF = outBI[[1]]
evalMCH = outBI[[2]]
evalMean = outBI[[3]]

mchcheck = ggplot(evalMCH, aes(x=A2023,y=F2023)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed", linewidth = 1) + 
  theme_bw() +   
  theme(axis.title = element_text(size=24),axis.text= element_text(size=14))+ 
  xlab(expression(Actual-MCH[2023])) +
  ylab(expression(Forecast-MCH[2023])) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")))

meancheck = ggplot(evalMean, aes(x=A2023,y=F2023)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed", linewidth = 1) + 
  theme_bw() +   
  theme(axis.title = element_text(size=24),axis.text= element_text(size=14))+ 
  xlab(expression(Actual-mu[2023])) +
  ylab(expression(Forecast-mu[2023])) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")))

withinrange = (FDF$STD >= FDF$STD2050L) & (FDF$STD <= FDF$STD2050H)

FDF$SStest = ifelse(withinrange,"Not Significant","Significant")
FDF$SS = ifelse(withinrange,
                paste("Not Significant:",as.character(table(FDF$SStest)[1]),sep=""),
                paste("Significant:",as.character(table(FDF$SStest)[2]),sep=" "))

TCplt = ggplot(FDF,aes(x=STD,y=STD2050,col=SS)) + geom_point() + geom_linerange(aes(ymin=STD2050L, ymax=STD2050H),alpha=0.4) + 
  geom_abline(intercept = 0, slope = 1, color = "gray", linetype = "dashed", linewidth = 1) + 
  theme_bw() +   theme(axis.title = element_text(size=24),
                       axis.text= element_text(size=14)) +
  labs(color = "95% Sig.",x=expression(sigma[2023]),y=expression(sigma[2050]),title="Tree Cover") +
  theme(legend.position = c(0.1, 0.6))

TCeval = gridExtra::grid.arrange(mchcheck,meancheck,ncol=2)

TCeval
TCplt


grid.arrange(BIeval,TCeval,nrow=2)

## 1000w and 1200h
grid.arrange(BIplt,TCplt,nrow=2)
