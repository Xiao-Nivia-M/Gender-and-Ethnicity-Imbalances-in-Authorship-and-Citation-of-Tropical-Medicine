#1. Library ----
library(tidyverse)
library(ggplot2)
library(ggsci)
library(webr)
library(dplyr)
library(gridExtra)
library(ggpubr)
library(patchwork)
library(vtable)
library("Hmisc")
#2. Data Importation----
load("df_articledata_Jan30.RData")


#3.Journal Summary----
# Read in dataset of mapping table with Journal Impact Factor 
JIF=as.data.frame((read.csv("Src/wosjournalmapv2.csv",header=T)))
colnames(JIF)[1]="SO"
data=merge(x=article.data,y=JIF, by="SO")

modeldata=data[,c("TC","DT","TI","AG","LS","PY","LT","MB","GC","AR","TPH",
      "AR_2","name3","language","JIF")]
colnames(modeldata)[13]="Journal"
modeldata$JIF[modeldata$JIF=="N/A"]=NA
modeldata$JIF[modeldata$JIF==""]=NA
modeldata$JIF=as.numeric(modeldata$JIF)
summary(modeldata$JIF)

journal.rank=as.data.frame(table(modeldata$Journal))
journal.rank=subset(journal.rank,journal.rank$Var1!="other")

ggplot(data=journal.rank, aes(x=Freq, y=reorder(Var1,+Freq))) +
  geom_bar(stat = "identity", color="White", fill="steelblue")+
  xlab("Publication No.")+
  ylab("Journal Names")+
  theme(axis.text.y = element_text(size=10,face="bold"),
        axis.text.x = element_text(size=10,face="bold"),
        plot.title = element_text(face = "bold"))+
  geom_text(aes(label=Freq),vjust=-0.3,size=3)
ggsave("~/switchdrive/DataAnalysis /Github_R_Link_Test/Outcome/Plot/Tropical Medicine Journals by Number of Publications.jpg",
       width = 15,
       height = 8)

#Document type
#Review[1:yes, 0:no]
modeldata$Review=NA
for(i in 1:nrow(modeldata)){
  if(modeldata$DT[i]=="Review"){
    modeldata$Review[i]=1
  }else{
    modeldata$Review[i]=0
  }
}

lable.dt=c(Review="1:Yes, 0:No")
label(modeldata$Review)=lable.dt

#4.Gender Data----
##1. Exclude unknown---- 
genderdata=modeldata[!(modeldata$AG=="UU"|modeldata$AG=="WU"|modeldata$AG=="MU"|modeldata$AG=="UM"|modeldata$AG=="UW"),]
##2. Gender Category label----
genderdata$MM=NULL
genderdata$MW=NULL
genderdata$WM=NULL
genderdata$WW=NULL
genderdata$laW=NULL
genderdata$laM=NULL

label.MM=c(MM="1:Yes, 0:No")
label.MW=c(MW="1:Yes, 0:No")
label.WM=c(WM="1:Yes, 0:No")
label.WW=c(WW="1:Yes, 0:No")
label.laW=c(laW="1:Yes, 0:No")
label.laM=c(laM="1:Yes, 0:No")


#MM[1:yes, 0:no]
for(i in 1:nrow(genderdata)){
  if(genderdata$AG[i]=="MM"){
    genderdata$MM[i]=1
  }else{
    genderdata$MM[i]=0
  }
}
label(genderdata$MM)=label.MM

#WM[1:yes, 0:no]
for(i in 1:nrow(genderdata)){
  if(genderdata$AG[i]=="WM"){
    genderdata$WM[i]=1
  }else{
    genderdata$WM[i]=0
  }
}
label(genderdata$WM)=label.WM

#WW[1:yes, 0:no]
for(i in 1:nrow(genderdata)){
  if(genderdata$AG[i]=="WW"){
    genderdata$WW[i]=1
  }else{
    genderdata$WW[i]=0
  }
}
label(genderdata$WW)=label.WW

#MW[1:yes, 0:no]
for(i in 1:nrow(genderdata)){
  if(genderdata$AG[i]=="MW"){
    genderdata$MW[i]=1
  }else{
    genderdata$MW[i]=0
  }
}
label(genderdata$MW)=label.MW

#laM[1:yes, 0:no]
for(i in 1:nrow(genderdata)){
  if(genderdata$AG[i]=="MM"|genderdata$AG[i]=="WM"){
    genderdata$laM[i]=1
  }else{
    genderdata$laM[i]=0
  }
}
label(genderdata$laM)=label.laM

#laW[1:yes, 0:no]
for(i in 1:nrow(genderdata)){
  if(genderdata$AG[i]=="MW"|genderdata$AG[i]=="WW"){
    genderdata$laW[i]=1
  }else{
    genderdata$laW[i]=0
  }
}
label(genderdata$laW)=label.laW


##3. Select variables----
genderdata=genderdata[,c("TC","LS","LT","MB","JIF","AG","Review","MM","WW","MW","WM","laM","laW")]
genderdata <- genderdata[complete.cases(genderdata), ]
genderdata$AG<- as.factor(genderdata$AG)
genderdata$Review=as.factor(genderdata$Review)
genderdata$MM=as.factor(genderdata$MM)
genderdata$WW=as.factor(genderdata$WW)
genderdata$MW=as.factor(genderdata$MW)
genderdata$WM=as.factor(genderdata$WM)

genderdata$laM=as.factor(genderdata$laM)
genderdata$laW=as.factor(genderdata$laW)

st(genderdata)

#5. Ethnicity Data----
##1. Exclude unknown---- 
racedata=modeldata[-grep("Unknown",modeldata$AR_2),]
## 2. Ethnicity Category----
#Africa[1:yes, 0:no]
for(i in 1:nrow(racedata)){
  if(str_detect(racedata$AR_2[i],"Africa")){
    racedata$Africa[i]="1"
  }else{
    racedata$Africa[i]=0
  }
}

#Asia[1:yes, 0:no]
for(i in 1:nrow(racedata)){
  if(str_detect(racedata$AR_2[i],"Asia")){
    racedata$Asia[i]=1
  }else{
    racedata$Asia[i]=0
  }
}
#EU[1:yes, 0:no]
for(i in 1:nrow(racedata)){
  if(str_detect(racedata$AR_2[i],"EU")){
    racedata$EU[i]=1
  }else{
    racedata$EU[i]=0
  }
}
#Anglophone[1:yes, 0:no]
for(i in 1:nrow(racedata)){
  if(str_detect(racedata$AR_2[i],"Anglophone")){
    racedata$Anglophone[i]=1
  }else{
    racedata$Anglophone[i]=0
  }
}

racedata=racedata[,c("TC","LS","LT","MB","JIF","AR_2","Review","Africa","Anglophone","EU","Asia")]
racedata$EthnicityGroup<- as.factor(racedata$AR_2)
racedata$Review=as.factor(racedata$Review)
racedata$Africa=as.factor(racedata$Africa)
racedata$Asia=as.factor(racedata$Asia)
racedata$EU=as.factor(racedata$EU)
racedata$Anglophone=as.factor(racedata$Anglophone)
racedata= subset(racedata,select= -c(AR_2) )

racedata <- racedata[complete.cases(racedata), ]
st(racedata)

#Bi-variable Testing: Gender-----

##Gender Category
g1 <- lm(log(TC+0.0001) ~ AG, data = genderdata)
summary(g1)


#Gender:MM
genderdata$MM=factor(genderdata$MM,levels = c("1","0"))
g2=lm(log(TC+0.0001)~MM,data = genderdata)
summary(g2)


#Gender:laM
genderdata$laM=factor(genderdata$laM,levels = c("1","0"))
g3=lm(log(TC+0.0001)~laM,data = genderdata)
summary(g3)

#Gender:laW
g4=lm(log(TC+0.0001)~laW,data = genderdata)
summary(g4)


#LS
g5=lm(log(TC+0.0001)~LS,data = genderdata)
summary(g5)
par(mfrow = c(2, 2))
plot(g5)


#LT
g6=lm(log(TC+0.0001)~LT,data = genderdata)
summary(g6)
#MB
g7=lm(log(TC+0.0001)~MB,data = genderdata)
summary(g7)

#review
g8=lm(log(TC+0.0001)~Review,data = genderdata)
summary(g8)


#Multi-variable Testing: Gender-----
gg1=lm(log(TC+0.0001)~AG+LS+LT+MB+Review+JIF,data = genderdata )
summary(gg1)

hist(genderdata$TC)

par(mfrow = c(2, 2))
plot(gg1)
par(mfrow = c(1, 1))


gg2=lm(log(TC+0.0001)~laM+LS+LT+MB+Review+JIF,data = genderdata)
summary(gg2)

gg3=lm(TC~laW+LS+LT+MB+Review+JIF,data = genderdata)
summary(gg3)

gg4=lm(log(TC+0.0001)~MM+LS+LT+MB+Review+JIF,data = genderdata)
summary(gg4)

#Bi-variable Testing: Race-----
racedata$EthnicityGroup=factor(racedata$EthnicityGroup, levels = c("EU+EU","Anglophone+Anglophone","Asia+Asia","Anglophone+EU", "Asia+EU",  
                                                                   "Asia+Anglophone", "EU+Anglophone","Africa+Asia","Africa+Anglophone",
                                                                   "Africa+Africa","Anglophone+Africa","Anglophone+Asia", "EU+Asia",
                                                                   "Africa+EU","EU+Africa","Asia+Africa"))     
                                                                 
r1=lm(log(TC+0.0001)~EthnicityGroup,data=racedata)
summary(r1)

r2=lm(log(TC+0.0001)~Africa,data=racedata)
summary(r2)

r3=lm(log(TC+0.0001)~Asia,data=racedata)
summary(r3)

racedata$EU=factor(racedata$EU,levels=c("1","0"))
r4=lm(log(TC+0.0001)~EU,data=racedata)
summary(r4)

r5=lm(log(TC+0.0001)~LS,data=racedata)
summary(r4)

racedata$Anglophone=factor(racedata$Anglophone,levels = c("1","0"))
r5=lm(log(TC+0.0001)~Anglophone,data=racedata)
summary(r5)

r6=lm(log(TC+0.0001)~LS,data=racedata)
summary(r6)

r7=lm(log(TC+0.0001)~LT,data=racedata)
summary(r7)

r8=lm(log(TC+0.0001)~MB,data=racedata)
summary(r8)


#Multi-variable Testing: race-----
rr1=lm(log(TC+0.0001)~EthnicityGroup+LS+LT+MB+Review+JIF,data=racedata)
summary(rr1)

rr2=lm(log(TC+0.0001)~Africa+LS+LT+MB+Review+JIF,data=racedata)
summary(rr2)

rr3=lm(log(TC+0.0001)~Asia+LS+LT+MB+Review+JIF,data=racedata)
summary(rr3)




