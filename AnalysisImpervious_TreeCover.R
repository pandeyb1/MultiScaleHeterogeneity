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

df = getFullData()
df$Type[df$Type == "Developed Land"] = "Built Infrastructure"
df$Type[df$Type == "Greenery"] = "Tree Cover"
thdf = getthdf(0.27,0.33)
df30 = df %>% filter(Scale==30)


### Within and Between Heterogeneity; 

## Between
pwidth = 13.208333
pheight = 6.972222
df30b = df30 %>% group_by(Year,Type) %>% summarize(Mean1 = mean(Mean), STD = sd(Mean))
df30b$Type[df30b$Type == "Greenery"] = "Tree Cover"
df30b$MCH = df30b$STD/sqrt(df30b$Mean1 * (1-df30b$Mean1))

b1 = ggplot(df30b,aes(x=Mean1,y=STD,color=Year)) + geom_point(size=3) +   
  theme_bw() + scale_color_viridis_c() + labs(title="Tree Cover")+  
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

for (i in 1:length(fullmsa)){
  subdf = df30 %>% filter(NAME == fullmsa[i]) %>% filter(Type == "Built Infrastructure")
  subdf$Mean0 = subdf$Mean - min(subdf$Mean)
  subdf$STD0 = subdf$STD - min(subdf$STD)
  subdf$R2 =  cor(subdf$Mean0,subdf$STD0)^2
  out1 = subdf
  subdf = df30 %>% filter(NAME == fullmsa[i]) %>% filter(Type == "Tree Cover")
  subdf$Mean0 = subdf$Mean - min(subdf$Mean)
  subdf$STD0 = subdf$STD - min(subdf$STD)
  subdf$R2 =  cor(subdf$Mean0,subdf$STD0)^2
  out2 = subdf
  out = rbind(out1,out2)
  pltdf1 = rbind(pltdf1,out)
}

w1 = ggplot(data=pltdf1,aes(x= Mean,y=STD0,group=NAME,color=R2)) + theme_bw() + 
facet_wrap(~Type) +    geom_line(stat="smooth", method="lm", alpha=0.5) + ylim(0,0.1) + 
  theme(axis.title = element_text(size=24),
        axis.text= element_text(size=14),
        legend.title = element_text(size=18),
        legend.text= element_text(size=16)) + 
  labs(title = "Within Heterogeneity (Linear Approximation) Across \n380 US Metropolitan Statistical Areas (MSAs)",
       x = expression(mu),
       y = expression("Normalized "*sigma)) + 
  theme(strip.background = element_rect(fill="white",color="white"),strip.text = element_text(size = 20)) 

### Supplementary plot on how sd changes with mean with slope estimate, across metropolitan areas of varying sizes.

pltdf2 = data.frame()

for (i in 1:length(fullmsa)){
  subdf = df30 %>% filter(NAME == fullmsa[i]) %>% filter(Type == "Built Infrastructure")
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

ggplot(pltdf2,aes(y=Slope,x=MeanM,color=R2)) + 
  geom_point()  + theme_bw() + 
  geom_line(stat="smooth", alpha=0.5) + facet_wrap(~Type,scales="free") + 
  theme(axis.title = element_text(size=24),
        axis.text= element_text(size=14),
        axis.title.y = element_text(angle=0,hjust=0.5,vjust=0.5),
        legend.title = element_text(size=18),
        legend.text= element_text(size=16)) +
  theme(strip.background = element_rect(fill="white",color="white"),strip.text = element_text(size = 20)) + 
  ylab(expression(frac(d*sigma,d*mu))) + xlab(expression(mu[mu[t]]))
  
  

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

###########################################

########## CORMAT MCH

### Correlation Matrix

allyears = unique(df30$Year)
cormat = matrix(NA,nrow=length(allyears),ncol=length(allyears))
for(i in allyears){
  for(j in allyears){
    idat = df30%>% filter(Year==i) %>% filter(Type=="Built Infrastructure")
    jdat = df30 %>% filter(Year==j) %>% filter(Type=="Built Infrastructure")
    ijdat = merge(idat,jdat,by="NAME")
    out = cor.test(ijdat$MCH.x,ijdat$MCH.y,use="complete.obs")$estimate
    out = as.numeric(out)
    cormat[i-1984,j-1984] = out
  }
}

colnames(cormat) = 1985:2023
rownames(cormat) = 1985:2023
diag(cormat)=NA
long_data = melt(cormat)
long_data$Type  = "Built Infrastructure"

cormat1 = matrix(NA,nrow=length(allyears),ncol=length(allyears))
for(i in allyears){
  for(j in allyears){
    idat = df30%>% filter(Year==i) %>% filter(Type=="Tree Cover")
    jdat = df30 %>% filter(Year==j)%>% filter(Type=="Tree Cover")
    ijdat = merge(idat,jdat,by="NAME")
    out = cor.test(ijdat$MCH.x,ijdat$MCH.y,use="complete.obs")$estimate
    out = as.numeric(out)
    cormat1[i-1984,j-1984] = out
  }
}

colnames(cormat1) = 1985:2023
rownames(cormat1) = 1985:2023
diag(cormat1)=NA
long_data1 = melt(cormat1)
long_data1$Type  = "Tree Cover"

longdata = rbind(long_data,long_data1)

ggplot(longdata, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + # Use geom_tile for a heatmap-like image
  scale_fill_viridis_c(limits = c(0, 1),name=expression(rho)) + 
  labs(x = "Years", y = "Years") +
  theme_bw() + # Use a clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~Type) + 
  theme(axis.title = element_text(size=24),
        axis.text= element_text(size=14),
        axis.title.y = element_text(angle=90,hjust=0.5,vjust=0.5),
        legend.title = element_text(size=18),
        legend.text= element_text(size=16)) +
  theme(strip.background = element_rect(fill="white",color="white"),strip.text = element_text(size = 20))

  
## Multi-scale Analysis

allyears = unique(df$Year)
MSAs = unique(df$NAME)
outdf = data.frame()

for (i in 1:length(MSAs)){
  for (j in 1:length(allyears)){
    subdf = df %>% filter((Year==allyears[j]) & (NAME==MSAs[i])) %>% filter(Type=="Built Infrastructure")
    subdf = subdf[complete.cases(subdf),]
    mod = lm(MCH~log(Scale),data=subdf)
    r2 = summary(mod)$r.squared
    slope = as.numeric(coefficients(mod)[2])
    
    subdf = df %>% filter((Year==allyears[j]) & (NAME==MSAs[i])) %>% filter(Type=="Tree Cover")
    subdf = subdf[complete.cases(subdf),]
    mod = lm(MCH~log(Scale),data=subdf)
    r2.1 = summary(mod)$r.squared
    slope.1 = as.numeric(coefficients(mod)[2])
    
    out = rbind(data.frame(Year= allyears[j],NAME=MSAs[i],R2 = r2,Slope=slope,Type="Built Infrastructure"),
                data.frame(Year= allyears[j],NAME=MSAs[i],R2 = r2.1,Slope=slope.1,Type="Tree Cover"))
    outdf = rbind(outdf,out)
  }
  
}

head(outdf)

ggplot(outdf,aes(y=Slope, x=as.factor(Year))) +
  geom_boxplot() + facet_wrap(~Type) + 
  labs(x = "Years", y = expression(frac(d[MCH],d[Scale]))) +
  theme_bw() + # Use a clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~Type,ncol=1) + 
  theme(axis.title = element_text(size=24),
        axis.text= element_text(size=14),
        axis.title.y = element_text(angle=0,hjust=0.5,vjust=0.5),
        legend.title = element_text(size=18),
        legend.text= element_text(size=16)) +
  theme(strip.background = element_rect(fill="white",color="white"),strip.text = element_text(size = 20))






ggplot(subdf,aes(x=Scale,y=MaxH)) + geom_point() + geom_smooth(method="lm") + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
               labels = trans_format("log10", math_format(10^.x))) + 
                theme_bw() + 
                 theme(axis.title = element_text(size=24),
                       axis.text= element_text(size=18)) + labs(y="MCH",x="Scale")

ggplot(LA,aes(x=Scale,y=MCH,color=as.factor(Year))) + geom_point() + 
  scale_color_viridis_d(name="Year") +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + theme_bw() + 
  theme(axis.title = element_text(size=24),
        axis.text= element_text(size=18))  + 
  labs(title="Los Angeles-Long Beach-Anaheim, CA",caption="Year: 1985 & 2023") + 
  theme(legend.position.inside=c(0.2,0.2),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16))

mod1 = lm(MCH~Year+log(Scale) + as.factor(NAME),data=df %>% filter(Type=="Built Infrastructure"))
mod2 = lm(MCH~Year+log(Scale) + as.factor(NAME),data=df %>% filter(Type=="Tree Cover"))



#stargazer(mod1,mod2,type="html",omit="NAME",out="/Users/9oy/Documents/Projects/DSFResearch/Paper1/Manuscript/Reg.doc")


## Prepare Data for Tract-Level Analysis

shp = "/Users/9oy/Documents/Data/US/Shape/MSA/nhgis0029_shape/nhgis0029_shapefile_tl2020_us_cbsa_2020/US_cbsa_2020.shp"
msa = read_sf(shp)
msa <- msa %>%
  filter(LSAD == "M1", !grepl("PR", NAME))
write_sf(msa,"/Users/9oy/Documents/Projects/DSFResearch/Paper1/Data/US_cbsa_2020_384.shp")

df30s = df30 %>% filter(Year==2023) %>% filter(Type=="Built Infrastructure")

df30s = merge(msa,df30s,by.x="NAME",by.y="NAME")


hist(df30s$`Built Infrastructure`+df30s$`Tree Cover`)

tc = (read_sf("/Users/9oy/Documents/Projects/DSFResearch/Paper1/Data/Tracts_US_cbsa_2020_380.shp")) ## This file was generated in QGIS, using spatial join and simple join, using tracts centroids. 

test = df %>% filter((Type=="Built Infrastructure") & (NAME=="Abilene, TX"))
test1 = test%>% group_by(NAME,Year,Scale) %>% summarize(Count = mean(Count),MCH = mean(MCH))
ggplot(data=test1,aes(x=Count,y=MCH,color=Scale)) + geom_point()
plot(test1$Count,test1$MCH,col=rgb(0.1,0.1,0.1,0.1))



