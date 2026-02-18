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
source("/Users/9oy/Documents/Projects/DSFResearch/Paper1/Script/SuppFuncs.R")
tc = read_sf("/Users/9oy/Documents/Projects/DSFResearch/Paper1/Data/Tracts_US_cbsa_2020_380.shp") ## This file was generated in QGIS, using spatial join and simple join, using tracts centroids. 
TractC = tc %>% as.data.frame(.) %>%  select(-c(geometry)) %>% group_by(MSAGISJOIN) %>% summarize(NTracts = n())

df = getFullTractData() ## Every row is a MSA here.
head(df)

thdf = getthdf(0,1)

shp = "/Users/9oy/Documents/Data/US/Shape/MSA/nhgis0029_shape/nhgis0029_shapefile_tl2020_us_cbsa_2020/US_cbsa_2020.shp"
msa = read_sf(shp)
msa <- msa %>%  filter(LSAD == "M1", !grepl("PR", NAME))

df = merge(df,msa %>% as.data.frame(.) %>% select(GISJOIN,NAME),by.x="NAME",by.y="GISJOIN")
colnames(df)[c(1,13)] = c("GISJOIN","NAME")

df30b = df %>% group_by(Year,Type) %>% summarize(Mean1 = mean(Sum/Count,na.rm=T), STD = sd(Sum/Count,na.rm=T))
df30b$MCH = df30b$STD/sqrt(df30b$Mean1 * (1-df30b$Mean1))

b1 = ggplot(df30b,aes(x=Mean1,y=STD,color=Year)) + geom_point(size=3) +   
  theme_bw() + scale_color_viridis_c() +
  theme(axis.title = element_text(size=24),
        axis.text= element_text(size=14),
        legend.title = element_text(size=18),
        legend.text= element_text(size=16)) + 
  labs(title = "Between Heterogeneity Across \n380 US Metropolitan Statistical Areas (MSAs)",
       x = expression(mu),
       y = expression(sigma)) +
  facet_wrap(~Type,scale='free') +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  theme(strip.background = element_rect(fill="white",color="white"),strip.text = element_text(size = 20)) +
  geom_point(size=3)
b1

## Within

fullmsa = unique(df$NAME)
pltdf1 = data.frame()
df30 = df
for (i in 1:length(fullmsa)){
  subdf = df30 %>% filter(NAME == fullmsa[i]) %>% filter(Type == "Built Infrastructure")
  if (sum(is.na(subdf$Sum))==0){
  subdf$Mean0 = subdf$Mean# - min(subdf$Mean,na.rm=T)
  subdf$STD0 = subdf$STD# - min(subdf$STD,na.rm=T)
  subdf$R2 =  cor(subdf$Mean0,subdf$STD0,use="complete.obs")^2
  out1 = subdf
  subdf = df30 %>% filter(NAME == fullmsa[i]) %>% filter(Type == "Tree Cover")
  subdf$Mean0 = subdf$Mean# - min(subdf$Mean,na.rm=T)
  subdf$STD0 = subdf$STD# - min(subdf$STD,na.rm=T)
  subdf$R2 =  cor(subdf$Mean0,subdf$STD0,use="complete.obs")^2
  out2 = subdf
  out = rbind(out1,out2)
  pltdf1 = rbind(pltdf1,out)
  }
}

pltdf1BI= pltdf1 %>% filter(Type=="Built Infrastructure")
coefBI = coefficients(lm(STD0 ~ MaxH-1,data=pltdf1BI))[1]
thdf = getthdf(0,0.7)
thdf$yBI = thdf$y * coefBI

w1 = ggplot(data=pltdf1BI,aes(x= Mean0,y=STD0)) + theme_bw() + 
geom_line(stat="smooth", method="lm", alpha=0.5,aes(group=NAME, color=R2)) +geom_line(data=thdf,aes(x=x,y=yBI))+ ylim(0,0.5) + xlim(0,0.7) + 
  labs(title = "Within Heterogeneity \nBuilt Infrastructure",
       x = expression(mu),
       y = expression(sigma)) + 
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20))

pltdf1TC= pltdf1 %>% filter(Type=="Tree Cover")
coefTC = coefficients(lm(STD0 ~ MaxH-1,data=pltdf1TC))[1]
thdf = getthdf(0,0.7)
thdf$yBI = thdf$y * coefTC

w1.1 = ggplot(data=pltdf1TC,aes(x= Mean0,y=STD0)) + theme_bw() + 
  geom_line(stat="smooth", method="lm", alpha=0.5,aes(group=NAME, color=R2)) +geom_line(data=thdf,aes(x=x,y=yBI))+ ylim(0,0.5) + xlim(0,0.7) + 
  labs(title = "\nTree Cover",
       x = expression(mu),
       y = expression(sigma)) + 
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20))


grid.arrange(w1,w1.1,ncol=2)
  
  


### Supplementary plot on how sd changes with mean with slope estimate, across metropolitan areas of varying sizes.

pltdf2 = data.frame()

for (i in 1:length(fullmsa)){
  subdf = df30 %>% filter(NAME == fullmsa[i]) %>% filter(Type == "Built Infrastructure")
  if (sum(is.na(subdf$Sum))==0){
  slope = summary(lm(STD~Mean,data=subdf))$coefficients[2,1]
  r2 = summary(lm(STD~Mean,data=subdf))$r.squared
  out1 = data.frame(fullmsa[i],slope,r2,mean(subdf$Mean),"Built Infrastructure")
  subdf = df30 %>% filter(NAME == fullmsa[i]) %>% filter(Type == "Tree Cover")
  slope = summary(lm(STD~Mean,data=subdf))$coefficients[2,1]
  r2 = summary(lm(STD~Mean,data=subdf))$r.squared
  out2 = data.frame(fullmsa[i],slope,r2,mean(subdf$Mean),"Greenery")
  colnames(out1) = c("NAME","Slope","R2","MeanM","Type")
  colnames(out2) = c("NAME","Slope","R2","MeanM","Type")
  out = rbind(out1,out2)
  out[,2:4] = apply(out[,2:4],2,as.numeric)
  pltdf2 = rbind(pltdf2,out)
  }
}

ggplot(pltdf2,aes(y=Slope,x=MeanM,color=R2)) + 
  geom_point()  + theme_bw() + 
  geom_line(stat="smooth", alpha=0.5) + facet_wrap(~Type,scales="free") + 
  theme(axis.title = element_text(size=24),
        axis.text= element_text(size=14),
        axis.title.y = element_text(angle=0,hjust=0.5,vjust=0.5),
        legend.title = element_text(size=18),
        legend.text= element_text(size=16)) +
  theme(strip.background = element_rect(fill="white",color="white"),strip.text = element_text(size = 20)) + 
  ylab(expression(frac(d*sigma,d*mu))) + xlab(expression(mu[mu[t]]))  + geom_smooth()


#### Mean Conditional Heterogeneity

## bwteen
modbwBI = (lm(MCH~Year,data=df30b %>% filter(Type=="Built Infrastructure")))
bwr2BI = summary(modbwBI)$r.squared
bwslopeBI = as.numeric(coefficients(modbwBI)[2])

modwTC = (lm(MCH~Year,data=df30b %>% filter(Type=="Tree Cover")))
wr2BTC = summary(modwTC)$r.squared
wslopeBTC = as.numeric(coefficients(modwTC)[2])

bwdf = data.frame("Built Infrastructure","Between",bwr2BI,bwslopeBI,confint(modbwBI)[2,1],confint(modbwBI)[2,2])
bwdf1 = data.frame("Tree Cover","Between",wr2BTC,wslopeBTC,confint(modwTC)[2,1],confint(modwTC)[2,2])
colnames(bwdf) =c("Type","Scale","R2","Slope","Low","High")
colnames(bwdf1) =c("Type","Scale","R2","Slope","Low","High")
bwdf = rbind(bwdf,bwdf1)  

## wtn


fullmsa = unique(df$NAME)
pltdf1 = data.frame()

## How does std vary with mean; calculate slope

for (i in 1:length(fullmsa)){
  subdf = df30 %>% filter(NAME == fullmsa[i]) %>% filter(Type == "Built Infrastructure")
  if (sum(is.na(subdf$Sum))==0){
  slope = summary(lm((MCH)~(Year),data=subdf))$coefficients[2,1]
  r2 = summary(lm((MCH)~(Year),data=subdf))$r.squared
  out1 = data.frame(fullmsa[i],slope,r2,mean(subdf$MCH),"Built Infrastructure")
  
  subdf = df30 %>% filter(NAME == fullmsa[i]) %>% filter(Type == "Tree Cover")
  slope = summary(lm((MCH)~(Year),data=subdf))$coefficients[2,1]
  r2 = summary(lm((MCH)~(Year),data=subdf))$r.squared
  out2 = data.frame(fullmsa[i],slope,r2,mean(subdf$MCH),"Tree Cover")
  
  colnames(out1) = c("NAME","Slope","R2","MeanM","Type")
  colnames(out2) = c("NAME","Slope","R2","MeanM","Type")
  out = rbind(out1,out2)
  out[,2:4] = apply(out[,2:4],2,as.numeric)
  pltdf1 = rbind(pltdf1,out)
  }
}

ggplot(data= pltdf1,aes(x=MeanM,y=Slope,col=R2)) + 
  geom_point() + theme_bw() + facet_wrap(~Type,scales="free") + 
  ylab(expression(frac("d"*MCH,d*t))) + xlab(expression(mu[MCH[t]])) +
  theme(axis.title = element_text(size=24),
        axis.text= element_text(size=14),
        axis.title.y = element_text(angle=0,hjust=0.5,vjust=0.5),
        legend.title = element_text(size=18),
        legend.text= element_text(size=16)) +
  theme(strip.background = element_rect(fill="white",color="white"),strip.text = element_text(size = 20))

summary(pltdf1 %>% filter(Type=="Built Infrastructure"))
summary(pltdf1 %>% filter(Type=="Tree Cover"))

#### 






