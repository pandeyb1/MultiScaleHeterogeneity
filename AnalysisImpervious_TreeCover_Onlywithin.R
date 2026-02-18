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

### Within Heterogeneity; 

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

#mod = (lm(STD~MaxH+as.factor(NAME)-1,data=df30))
mod1 = (lm(STD~MaxH-1,data=df30%>%filter(Type=="Built Infrastructure")))
smod1 = summary(mod1)
coefmod1 = coefficients(mod1)[1]

mod2 = (lm(STD~MaxH-1,data=df30%>%filter(Type=="Tree Cover")))
smod2 = summary(mod2)
coefmod2 = coefficients(mod2)[1]

thdf1 = getthdf(0,max(df30$Mean))
thdf1$yfit = thdf1$y * coefmod1
thdf1$Type = "Built Infrastructure"

thdf2 = getthdf(0,max(df30$Mean))
thdf2$yfit = thdf2$y * coefmod2
thdf2$Type = "Tree Cover"

thdf = rbind(thdf1,thdf2)

# Label plot with MCH

dat_text <- data.frame(
  label = c("MCH=0.66", "MCH=0.73"),
  Type   = c("Built Infrastructure","Tree Cover")
)


w0 = ggplot(data=df30,aes(x= Mean,y=STD,group=NAME)) + theme_bw() + 
  facet_wrap(~Type) +    geom_line(stat="smooth", method="lm", alpha=0.5) +
  theme(axis.title = element_text(size=24),
        axis.text= element_text(size=14)) + 
  labs(title = "Within Heterogeneity (Linear Approximation) Across \n380 US Metropolitan Statistical Areas (MSAs)",
       x = expression(mu),
       y = expression(sigma)) + 
  theme(strip.background = element_rect(fill="white",color="white"),strip.text = element_text(size = 20)) + 
  theme(plot.margin = unit(c(0.1,0.5, 0.2, 0.2), "cm")) +
  geom_line(data=thdf,aes(x=x,y=yfit,group=Type),color="blue")

w0 + geom_text(
    data    = dat_text,
    mapping = aes(x = -Inf, y = -Inf, label = label),
    hjust   = -3,
    vjust   = -1,group=NA,size=5)


## Normalized sigma

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

fullmsa = unique(df$NAME)
pltdf1 = data.frame()

## How does std vary with mean; calculate slope

for (i in 1:length(fullmsa)){
  subdf = df30 %>% filter(NAME == fullmsa[i]) %>% filter(Type == "Built Infrastructure")
  slope = summary(lm((MCH)~(Year),data=subdf))$coefficients[2,1]
  r2 = summary(lm((MCH)~(Year),data=subdf))$r.squared
  out1 = data.frame(fullmsa[i],slope,r2,subdf[subdf$Year==1985,"MCH"],"Built Infrastructure")
  
  subdf = df30 %>% filter(NAME == fullmsa[i]) %>% filter(Type == "Tree Cover")
  slope = summary(lm((MCH)~(Year),data=subdf))$coefficients[2,1]
  r2 = summary(lm((MCH)~(Year),data=subdf))$r.squared
  out2 = data.frame(fullmsa[i],slope,r2,subdf[subdf$Year==1985,"MCH"],"Tree Cover")
  
  colnames(out1) = c("NAME","Slope","R2","MeanM","Type")
  colnames(out2) = c("NAME","Slope","R2","MeanM","Type")
  out = rbind(out1,out2)
  out[,2:4] = apply(out[,2:4],2,as.numeric)
  pltdf1 = rbind(pltdf1,out)
}

head(pltdf1)
pltdf1$final = (pltdf1$MeanM + 38 * pltdf1$Slope) - pltdf1$MeanM

ggplot(data= pltdf1,aes(x=MeanM,y=final)) + geom_density_2d_filled(alpha=0.6) + 
  geom_density_2d(linewidth = 0.25, colour = "black")+
  geom_point(alpha=0.15,size=2,col="white") + 
  geom_hline(yintercept=0,col="white") + 
theme_bw() + facet_wrap(~Type) + 
  ylab(expression(Delta * MCH[2023-1985])) + xlab(expression(MCH[1985])) +
  theme(axis.title = element_text(size=24),
        axis.text= element_text(size=14),
        axis.title.y = element_text(angle=90,hjust=0.5,vjust=0.5),
        legend.title = element_text(size=18),
        legend.text= element_text(size=16)) +
  theme(strip.background = element_rect(fill="white",color="white"),strip.text = element_text(size = 20))

bivec = pltdf1 %>% filter(Type=="Built Infrastructure")
percentiles <- quantile(bivec$Slope * 38, probs = c(0.05, 0.95))
lower_bound <- percentiles["5%"]
upper_bound <- percentiles["95%"]
# Print the results
print(paste("Median:", median(bivec$Slope*38)))
print(paste("5th percentile:", lower_bound))
print(paste("95th percentile:", upper_bound))
print(paste("95th percentile range:", round(lower_bound,3), "to", round(upper_bound,3)))


bivec = pltdf1 %>% filter(Type=="Tree Cover")
percentiles <- quantile(bivec$Slope * 38, probs = c(0.05, 0.95))
lower_bound <- percentiles["5%"]
upper_bound <- percentiles["95%"]
# Print the results
print(paste("Median:", median(bivec$Slope*38)))
print(paste("5th percentile:", lower_bound))
print(paste("95th percentile:", upper_bound))
print(paste("95th percentile range:", round(lower_bound,3), "to", round(upper_bound,3)))


ggplot(pltdf1,aes(Slope * 38,fill=Type))+
  scale_fill_manual(values=c("red","blue"))+
  geom_histogram(alpha=0.5,binwidth=0.01,position="identity") + theme_bw() + 
  theme(axis.title = element_text(size=24),
        axis.text= element_text(size=14),
        axis.title.y = element_text(angle=90,hjust=0.5,vjust=0.5),
        legend.title = element_text(size=18),
        legend.text= element_text(size=16)) +
  xlab(expression(Delta * MCH[2023-1985])) + 
  ylab("Count")
  
## put changes in MCH in the context of variations across MCH

df30 %>% filter(Type=="Built Infrastructure") %>% filter(Year==2023) %>% summary(.)
df30 %>% filter(Type=="Tree Cover") %>% filter(Year==2023) %>% summary(.)
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
  scale_fill_viridis_c(name=expression(rho)) + 
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

library(usethis) 
usethis::edit_r_environ()

df1 = merge(df,)

UGgr = read.csv("/Users/9oy/Documents/Projects/DSFResearch/Paper1/Data/UrbanGrowth_Imp.csv")[,-c(1)]
df1 = merge(df,UGgr,by="NAME",all.x=T)

mod1 = lm(MCH~log(Scale) + as.factor(NAME),data=df1 %>% filter(Type=="Built Infrastructure"))
mod1.1 = lm(MCH~Year + as.factor(NAME),data=df1 %>% filter(Type=="Built Infrastructure"))
mod1.2 = lm(MCH~Year+log(Scale) + as.factor(NAME),data=df1 %>% filter(Type=="Built Infrastructure"))
mod1.3 = lm(MCH~Year+log(Scale) + slope,data=df1 %>% filter(Type=="Built Infrastructure"))


mod2 = lm(MCH~log(Scale) + as.factor(NAME),data=df1 %>% filter(Type=="Tree Cover"))
mod2.1 = lm(MCH~Year + as.factor(NAME),data=df1 %>% filter(Type=="Tree Cover"))
mod2.2 = lm(MCH~Year+log(Scale) + as.factor(NAME),data=df1 %>% filter(Type=="Tree Cover"))
mod2.3 = lm(MCH~Year+log(Scale) + slope,data=df1 %>% filter(Type=="Tree Cover"))


stargazer(mod1,mod1.1,mod1.2,mod1.3,mod2,mod2.1,mod2.2,mod2.3,type="text",omit="NAME")
stargazer(mod1.3,mod2.3,type="text",omit="NAME")
#stargazer(mod1,mod1.1,mod1.2,mod2,mod2.1,mod2.2,type="text",omit="NAME")
#stargazer(mod1,mod1.1,mod1.2,mod2,mod2.1,mod2.2,type="html",omit="NAME",out="/Users/9oy/Documents/Projects/DSFResearch/Paper1/Manuscript/Regv1.doc")
#stargazer(mod1,mod2,type="html",omit="NAME",out="/Users/9oy/Documents/Projects/DSFResearch/Paper1/Manuscript/Regv1.doc")
#stargazer(mod1.2,mod2.2,type="html",omit="NAME",out="/Users/9oy/Documents/Projects/DSFResearch/Paper1/Manuscript/Reg.doc")


BI=df %>% filter(Type=="Built Infrastructure") %>% filter(Scale==30) %>% filter(Year==2023)
GC = df %>% filter(Type=="Tree Cover")%>% filter(Scale==30)%>% filter(Year==2023)
BIGC = merge(BI,GC,by=c("NAME","Scale","Year"))
plot(BIGC$MCH.x,BIGC$MCH.y)

