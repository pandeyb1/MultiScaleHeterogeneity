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
setwd("/Users/9oy/Documents/Projects/DSFResearch/Paper1/Data/Impervious/Impervious")

files = list.files(pattern = "*.xlsx")

# msa = read_sf("/Users/9oy/Documents/Data/US/Shape/MSA/nhgis0029_shape/nhgis0029_shapefile_tl2020_us_cbsa_2020/US_cbsa_2020.shp")
# msa = msa[(msa$LSAD == "M1"),]
# msa = msa[!grepl( "PR", msa$NAME, fixed = TRUE),]
# allmetros = msa$NAME
# allmetrosxlsx = paste(allmetros,".xlsx",sep="")
# length(allmetrosxlsx)

df = data.frame()

for (i in 1:length(files)){
  subdf = read_excel(files[i],.name_repair = "minimal")
  df = rbind(df,subdf)
}

df$Mean = as.numeric(df$Mean)
colnames(df)[1] = "ID"

df$MCH = df$STD/sqrt(df$Mean * (1-df$Mean))
df$MaxH = sqrt(df$Mean * (1-df$Mean))



plt1 = ggplot(df, aes(x = Mean, y = STD, col=Year)) + scale_color_viridis_c() + 
  geom_point(alpha = 0.15,size=2) +
  theme_bw() +
  labs(title = expression("Developed Land: " * mu * " versus " * sigma),
       subtitle = "380 US Metropolitan Statistical Areas (MSAs)",
       x = expression(mu),
       y = expression(sigma)) #+ # theme(legend.position="none")

#plt1

x = seq(0,1,0.001)
y = sqrt(x * (1-x))
thdf = data.frame(x,y)

b_reg = lm(STD~MaxH - 1,data=df)
summary(b_reg)
x_avgbline = x
y_avgbline = predict(b_reg,newdata=data.frame(MaxH=y))
abgblinedf = data.frame(x = x_avgbline,y = y_avgbline)

p1 = plt1 + geom_line(data=thdf,aes(x=x,y=y),color="red",linewidth=2) + xlim(0,0.8) + ylim(0,0.55) + 
  theme(axis.title = element_text(size=24),
        axis.text= element_text(size=18),
        legend.title = element_text(size=18),
        legend.text= element_text(size=16)) + geom_line(data=abgblinedf,aes(x=x,y=y),color="blue",linewidth=2) +
  annotate("text",x=0.1, y=0.5,label=paste("MCH=",as.character(round(0.2903537,2)),sep=""),color="blue",size=6)

#p1

ggsave("/Users/9oy/Documents/Projects/DSFResearch/Paper1/Figures/IFig1_380MSAsv1.jpg",p1)

###############
###################################################################


df30 = df %>% filter(Scale==30)

plt2 = ggplot(df30, aes(x = Mean, y = STD, col=Year)) + scale_color_viridis_c() + 
  geom_point(alpha = 0.15,size=2) +
  theme_bw() +
  labs(title = expression("Developed Land: " * mu * " versus " * sigma),
       subtitle = "380 US Metropolitan Statistical Areas (MSAs), 30 m",
       x = expression(mu),
       y = expression(sigma)) #+ # theme(legend.position="none")
x = seq(0,1,0.001)
y = sqrt(x * (1-x))
thdf = data.frame(x,y)

b_reg = lm(STD~MaxH-1,data=df30)
summary(b_reg)
x_avgbline = x
y_avgbline = predict(b_reg,newdata=data.frame(MaxH=y))
abgblinedf = data.frame(x = x_avgbline,y = y_avgbline)

p2 = plt2 + geom_line(data=thdf,aes(x=x,y=y),color="red",linewidth=2,alpha=0.5) + xlim(0,0.8) + ylim(0,0.55) + 
  theme(axis.title = element_text(size=24),
        axis.text= element_text(size=18),
        legend.title = element_text(size=18),
        legend.text= element_text(size=16)) + 
  geom_line(data=abgblinedf,aes(x=x,y=y),color="blue",linewidth=2,alpha=0.5) +
  annotate("text",x=0.6, y=0.2,label=paste("MCH=",as.character(round(0.6557507,2)),sep=""),color="blue",size=6)

p2
ggsave("/Users/9oy/Documents/Projects/DSFResearch/Paper1/Figures/IFig2_380MSAs_30m.jpg",p2)

#######################################################################################################
MCHm85 = round(df30 %>% filter(Year==1985) %>% summarize(out = mean(MCH)),2)
MCHm23 = round(df30 %>% filter(Year==2023) %>% summarize(out = mean(MCH)),2)


p3 = ggplot(df30,aes(x = MCH)) + geom_histogram(bins=50,colour = 1, fill = "white") +
  theme_bw() +  theme(axis.title = element_text(size=24),
                      axis.text= element_text(size=18)
  )  + ylab("Count") + xlab("Mean-Conditional Heterogeneity")

p3.1 = p3 + geom_histogram(data=df30 %>% filter(Year==1985),alpha=0.5,fill="blue") + 
  geom_histogram(data=df30 %>% filter(Year==2023),alpha=0.5,fill="red") + 
  annotate("text",x=0.55, y=900,label=paste("MCH-1985=",as.character(round(MCHm85,2)),sep=""),color="blue",size=6) + 
  annotate("text",x=0.55, y=800,label=paste("MCH-2023=",as.character(round(MCHm23,2)),sep=""),color="red",size=6)
p3.1
ggsave("/Users/9oy/Documents/Projects/DSFResearch/Paper1/Figures/IFig3_380MSAs_30m_hist.jpg",p3.1)

#######################################################################################################

allmsas = unique(df$NAME)
out= data.frame()
for(i in 1:length(allmsas)){
  subdf = subset(df,(df$NAME == allmsas[i]) & (df$Scale == 30))
  subdat = ts(subdf$MCH,start=1985,end=2023,frequency=1)
  mkres = mk.test(subdat)
  tau = as.numeric(mkres$estimates[3])
  tau_pval = mkres$p.value
  ssloperes= sens.slope(subdat)
  sslope = as.numeric(ssloperes$estimates)
  s_pval = ssloperes$p.value
  outdat = cbind(tau,tau_pval,sslope,s_pval)
  out = rbind(out,outdat)
}
colnames(out) = c("tau","tpval","sslope","spval")
out$NAME = allmsas
out$tau[out$tpval>0.05] = 0
out$sslope[out$spval>0.05] = 0
mean(out$sslope)
p4 = ggplot(out,aes(x=tau,y=sslope)) + geom_point() + 
  theme_bw() + xlab(expression(tau)) + ylab("Sen's Slope") + 
  theme(axis.title = element_text(size=24),
        axis.text= element_text(size=18))   +  geom_text_repel(
          aes(label = NAME),
          size = 2,
          color = "grey50"
        )

p4.1 = ggMarginal(p4, type="histogram")
p4.1

ggsave("/Users/9oy/Documents/Projects/DSFResearch/Paper1/Figures/IFig4_380MSAs_30m_Change.jpg",p4.1)

sum(out$tau > 0)
sum(out$sslope > 0)


########## CORMAT

### Correlation Matrix

allyears = unique(df30$Year)
cormat = matrix(NA,nrow=length(allyears),ncol=length(allyears))
for(i in allyears){
  for(j in allyears){
    idat = df30%>% filter(Year==i)
    jdat = df30 %>% filter(Year==j)
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
ggplot(long_data, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + # Use geom_tile for a heatmap-like image
  scale_fill_gradient(low = "white", high = "red") + # Customize color gradient
  labs(x = "Column Name", y = "Row Name", title = "Developed Land Heterogeneity: Hysteresis") + # Add labels and title
  theme_minimal() + # Use a clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability

LA = df %>% filter(Scale==30) 
ggplot(data=LA %>% filter(NAME=="Los Angeles-Long Beach-Anaheim, CA"),aes(y=STD,x=Mean,col=Year)) +
  geom_point()

fullmsa = unique(df$NAME)
pltdf1 = data.frame()

for (i in 1:length(fullmsa)){
  subdf = df %>% filter(Scale ==30) %>% filter(NAME == fullmsa[i]) #%>% filter(Year %in% c(1985,2023))
  subdf$meanmin = subdf$Mean - min(subdf$Mean) +0.0001
  subdf$stdmin = subdf$STD - min(subdf$STD) + 0.0001
  
  pltdf1 = rbind(pltdf1,subdf)
}

ggplot(data=pltdf1,aes(x=log(Mean),y=log(STD),col=NAME,group=NAME)) +
  geom_point(size=0.01) + theme_bw() + 
  geom_smooth(method="lm",se=F) + theme(legend.position = "None") 



plot(STD~Mean,data=LA%>%filter(Scale==30))