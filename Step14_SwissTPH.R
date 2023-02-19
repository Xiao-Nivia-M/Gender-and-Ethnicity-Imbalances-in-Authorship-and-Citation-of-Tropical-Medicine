#1. Library----
library("writexl")
library(tidyverse)

#2. Data Importation----
load("Outcome/ResultingVariables/Step4.1_DataProcessing_TPH/df_1009_TPH_final.Rdata")
load("df7_articledata_withraces.RData")

#3. Combine TPH data----
article.data$TPH= ((tolower(article.data$TI))%in% (df_1009_TPH_final$TI))
save(article.data,file="df14_articledata_tph.RData")

#4. Data Overview----
load("Outcome/SourceData/ImportedRowData/df_1009_TPH_1528.Rdata")

# 5.Swiss TPH Authorship: Gender and Race-----
## 5.1 Gender:Swiss TPH Gender Categories in Authorship----
summary(article.data$TPH==TRUE)
article.tph.data=article.data[article.data$TPH==TRUE,]
article.tph.gender.data=article.tph.data[which(!is.na(article.tph.data$GC)),]
table(article.tph.gender.data$AG)

df.tph=as.data.frame(table(article.tph.gender.data$AG))
colnames(df.tph)=c("GenderCategories","Frequency")
ggplot(data=df.tph, aes(x=reorder(GenderCategories,-Frequency), y=Frequency)) +
  geom_bar(stat="identity",fill=Color)+
  theme_minimal()+
  ggtitle("Swiss TPH Gender Assignment Overview")+
  theme(axis.text.x = element_text(size=8,face="bold"),
        plot.title = element_text(face = "bold"))+
  geom_text(aes(label=Frequency),vjust=-0.3,size=3)+
  xlab("Gender Category")+
  ylab("Frequency")


###fa_gender-----
table(article.tph.data$fa_gender)
###la_gender----
table(article.tph.data$la_gender)


## 5.2. Gender: Swiss TPH Temporal Gender Category Change----
timedata.tph=NULL
for(i in unique(article.tph.data$PY)){
  subgends=article.tph.data$AG[article.tph.data$PY==i & 
                                 article.tph.data$AG%in%c("MM","WM","MW","WW")]
  subgends=na.omit(subgends)
  timedata.tph.i=data.frame(Year=rep(i,4),Gender=c("MM","WM","MW","WW"),
                            Prop=c(mean(subgends=="MM"),
                                   mean(subgends=="WM"),
                                   mean(subgends=="MW"),
                                   mean(subgends=="WW")),
                            Color=c("darkslateblue","darkslategray4",
                                    "lightcyan3","lightsalmon3"))
  timedata.tph=rbind(timedata.tph,timedata.tph.i)
}
timedata.tph$Color=as.character(timedata.tph$Color)
timedata.tph=na.omit(timedata.tph)

w.tphdata=timedata.tph[timedata.tph$Gender%in%c("WM","MW","WW"),]
write_xlsx(w.tphdata,"wtph.xlsx")

f1plot(timedata.tph,"Overall Swiss TPH Authorship by Year") #not shown in paper. 

## 5.3: Race: Swiss TPH Race Distribution----
View(article.tph.data)
article.tph.data$AR
authorship=data.frame(matrix(nrow=nrow(article.tph.data),ncol=11))
colnames(authorship)=c("E.Asia","E.Europe","E.SpeakingRegions","N.Africa",
                       "N.Europe","S.E.Asia","S.Asia","S.Europe","SSA",
                       "W.Europe","W.Asia")
authorship$AR=article.tph.data$AR

# Exclude "Unknown"
for(i in 1:nrow(authorship)){
  if(str_detect(authorship$AR[i], "Unknown")){
    authorship$exclusion[i]= 1 #exclude
  }else{
    authorship$exclusion[i]= 0 #include
  }
}

#Added plot: Overview
O1=as.data.frame(table(authorship$exclusion))
O1$Category[1]="w/o unknown"
O1$Category[2]="w/ unknown"

ggplot(data=O1, aes(x=Category,y=Freq))+
  geom_bar(stat = "identity",fill=c("steelblue","gray"))+
  xlab("Origin Region Category")+
  ylab("Frequency")+
  ggtitle("TPH Origin Region Assignment Overview")+
  theme(axis.text.x = element_text(size=8,face="bold"),
        plot.title = element_text(face = "bold"))+
  geom_text(aes(label=Freq),vjust=-0.3,size=3)


#### 1. Eastern Asia 
for(i in 1:nrow(authorship)){
  if(str_detect(authorship$AR[i], "Eastern Asia")){
    authorship$E.Asia[i]= 1
  }else{
    authorship$E.Asia[i]= 0
  }
}

#### 2. Eastern Europe 
for(i in 1:nrow(authorship)){
  if(str_detect(authorship$AR[i], "Eastern Europe")){
    authorship$E.Europe[i]= 1
  }else{
    authorship$E.Europe[i]= 0
  }
}

#### 3. EnglishSpeaking Regions
for(i in 1:nrow(authorship)){
  if(str_detect(authorship$AR[i], "EnglishSpeaking Regions")){
    authorship$E.SpeakingRegions[i]= 1
  }else{
    authorship$E.SpeakingRegions[i]= 0
  }
}

#### 4. Northern Africa
for(i in 1:nrow(authorship)){
  if(str_detect(authorship$AR[i], "Northern Africa")){
    authorship$N.Africa[i]= 1
  }else{
    authorship$N.Africa[i]= 0
  }
}

#### 5. Northern Europe
for(i in 1:nrow(authorship)){
  if(str_detect(authorship$AR[i], "Northern Europe")){
    authorship$N.Europe [i]= 1
  }else{
    authorship$N.Europe[i]= 0
  }
}

#### 6. South-eastern Asia
for(i in 1:nrow(authorship)){
  if(str_detect(authorship$AR[i], "South-eastern Asia")){
    authorship$S.E.Asia[i]= 1
  }else{
    authorship$S.E.Asia[i]= 0
  }
}

#### 7. Southern Asia
for(i in 1:nrow(authorship)){
  if(str_detect(authorship$AR[i], "Southern Asia")){
    authorship$S.Asia[i]= 1
  }else{
    authorship$S.Asia[i]= 0
  }
}

#### 8. Southern Europe
for(i in 1:nrow(authorship)){
  if(str_detect(authorship$AR[i], "Southern Europe")){
    authorship$S.Europe [i]= 1
  }else{
    authorship$S.Europe[i]= 0
  }
}

#### 9. Sub-Saharan Africa
for(i in 1:nrow(authorship)){
  if(str_detect(authorship$AR[i], "Sub-Saharan Africa")){
    authorship$SSA[i]= 1
  }else{
    authorship$SSA[i]= 0
  }
}

#### 10. Western Europe
for(i in 1:nrow(authorship)){
  if(str_detect(authorship$AR[i], "Wesrtern Europe")){
    authorship$W.Europe[i]= 1
  }else{
    authorship$W.Europe[i]= 0
  }
}

#### 11. Western Asia
for(i in 1:nrow(authorship)){
  if(str_detect(authorship$AR[i], "Western Asia")){
    authorship$W.Asia[i]= 1
  }else{
    authorship$W.Asia[i]= 0
  }
}

### 5.3.1 with unknown:summary----
summary=data.frame(matrix(nrow=11,ncol=2))
colnames(summary)= c("Region","Freq")
summary$Region=c("E.Asia","E.Europe","E.SpeakingRegions","N.Africa",
                 "N.Europe","S.E.Asia","S.Asia","S.Europe","SSA",
                 "W.Europe","W.Asia")
summary$Fill=c("darkslateblue","darkslategray4","lightsalmon3",
               "lightcyan3","darkslategray4","darkslateblue",
               "darkslateblue","darkslategray4","lightcyan3",
               "darkslategray4","darkslateblue")

summary[1,2]=sum(authorship$E.Asia)
summary[2,2]=sum(authorship$E.Europe)
summary[3,2]=sum(authorship$E.SpeakingRegions)
summary[4,2]=sum(authorship$N.Africa)
summary[5,2]=sum(authorship$N.Europe)
summary[6,2]=sum(authorship$S.E.Asia)
summary[7,2]=sum(authorship$S.Asia)
summary[8,2]=sum(authorship$S.Europe)
summary[9,2]=sum(authorship$SSA)
summary[10,2]=sum(authorship$W.Europe)
summary[11,2]=sum(authorship$W.Asia)

### 5.3.2 with unknown:visualization----
tph1=ggplot(data=summary,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill=summary$Fill) +
  xlab("Region")+
  ylab("Paper No.")+
  ggtitle("Swiss TPH Race Assignment Overview w/ unknown")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)+
  theme(axis.text.x = element_text(size=8,face="bold"),
        plot.title = element_text(face = "bold"))
### 5.3.4 without unknown:summary----
authorship.k=subset(authorship,authorship$exclusion==0)

summary.k=data.frame(matrix(nrow=11,ncol=2))
colnames(summary.k)= c("Region","Freq")
summary.k$Region=c("E.Asia","E.Europe","E.SpeakingRegions","N.Africa",
                   "N.Europe","S.E.Asia","S.Asia","S.Europe","SSA",
                   "W.Europe","W.Asia")
summary.k$Fill=c("darkslateblue","darkslategray4","lightsalmon3",
                 "lightcyan3","darkslategray4","darkslateblue",
                 "darkslateblue","darkslategray4","lightcyan3",
                 "darkslategray4","darkslateblue")
summary.k[1,2]=sum(authorship.k$E.Asia)
summary.k[2,2]=sum(authorship.k$E.Europe)
summary.k[3,2]=sum(authorship.k$E.SpeakingRegions)
summary.k[4,2]=sum(authorship.k$N.Africa)
summary.k[5,2]=sum(authorship.k$N.Europe)
summary.k[6,2]=sum(authorship.k$S.E.Asia)
summary.k[7,2]=sum(authorship.k$S.Asia)
summary.k[8,2]=sum(authorship.k$S.Europe)
summary.k[9,2]=sum(authorship.k$SSA)
summary.k[10,2]=sum(authorship.k$W.Europe)
summary.k[11,2]=sum(authorship.k$W.Asia)
### 5.3.5 without unknown:visualization----
tph2=ggplot(data=summary.k,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill=summary.k$Fill) +
  xlab("Region")+
  ylab("Paper No.")+
  ggtitle("Swiss TPH Race Assignment Overview w/o unknown")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)+
  theme(axis.text.x = element_text(size=8,face="bold"),
        plot.title = element_text(face = "bold"))

tph1/tph2

#6. Swiss TPH Citation: Gender and Race-----
##6.1. Gender----
###6.1.1 Gender: without self-citation-----
gender.tph.df=data.frame(Year=NA,MM=NA,WM=NA,MW=NA,WW=NA)
i=0
for (x in which(article.data$TPH==TRUE)){
  cited.papers=strsplit(article.data$CP[x],", ")[[1]]
  self.authored=strsplit(article.data$SA[x],", ")[[1]]
  cited.notself=as.numeric(cited.papers[!(cited.papers%in%self.authored)])
  cited.notself=cited.notself[!is.na(article.data$GC[cited.notself])]
  i=i+1
  if(length(cited.notself)>0){
    cited.genders=article.data$GC[cited.notself]
    gender.table=table(factor(cited.genders,lev=0:3))
    gender.tph.df[i,1]=article.data$PY[x]
    gender.tph.df[i,2:5]=as.vector(gender.table)
    
  }else{
    gender.table=table(factor(NA,lev=0:3))
    gender.tph.df[i,1]=article.data$PY[x]
    gender.tph.df[i,2:5]=as.vector(gender.table)
  }
}
summary(gender.tph.df)

sum.tph.df=data.frame(GenderCategories=NA,Frequency=NA)
sum.tph.df[1,]=c("MM",sum(gender.tph.df$MM))
sum.tph.df[2,]=c("WM",sum(gender.tph.df$WM))
sum.tph.df[3,]=c("MW",sum(gender.tph.df$MW))
sum.tph.df[4,]=c("WW",sum(gender.tph.df$WW))

sum.tph.df$Frequency=as.numeric(sum.tph.df$Frequency)

swisstph.cited=ggplot(data=sum.tph.df, aes(x=reorder(GenderCategories,-Frequency), y=Frequency,
                                           label=Frequency)) +
  geom_bar(stat="identity",fill=Color)+
  theme_minimal()+
  #geom_text(size = 3, position = position_stack(vjust = 0.5))+
  ggtitle("Swiss TPH Gender Categories in Citaion (without self-citation)")+
  xlab("Gender Category")+
  ylab("Frequency")+
  geom_text(aes(label=Frequency),vjust=-0.3,size=3)+
  theme(axis.text.x = element_text(size=8,face="bold"),
        plot.title = element_text(face = "bold"))

swisstph.cited #section 3.4.3.1

###6.1.2  Gender: with self-citation----
self.gender.tph.df=data.frame(Year=NA,MM=NA,WM=NA,MW=NA,WW=NA)
i=0
for (x in which(article.data$TPH==TRUE)){
  cited.papers=strsplit(article.data$CP[x],", ")[[1]]
  cited.w.self=as.numeric(cited.papers)
  cited.w.self=cited.w.self[!is.na(article.data$GC[cited.w.self])]
  i=i+1
  if(length(cited.w.self)>0){
    cited.genders=article.data$GC[cited.w.self]
    gender.table=table(factor(cited.genders,lev=0:3))
    self.gender.tph.df[i,1]=article.data$PY[x]
    self.gender.tph.df[i,2:5]=as.vector(gender.table)
  }else{
    gender.table=table(factor(NA,lev=0:3))
    self.gender.tph.df[i,1]=article.data$PY[x]
    self.gender.tph.df[i,2:5]=as.vector(gender.table)
  }
}

summary(self.gender.tph.df)

sum.tph.self.df=data.frame(GenderCategories=NA,Frequency=NA)
sum.tph.self.df[1,]=c("MM",sum(self.gender.tph.df$MM))
sum.tph.self.df[2,]=c("WM",sum(self.gender.tph.df$WM))
sum.tph.self.df[3,]=c("MW",sum(self.gender.tph.df$MW))
sum.tph.self.df[4,]=c("WW",sum(self.gender.tph.df$WW))

sum.tph.self.df$Frequency=as.numeric(sum.tph.self.df$Frequency)

swisstph.self.cited=ggplot(data=sum.tph.self.df, aes(x=reorder(GenderCategories,-Frequency), 
                                                     y=Frequency,label=Frequency)) +
  geom_bar(stat="identity",fill=Color)+
  theme_minimal()+
  #geom_text(size = 3, position = position_stack(vjust = 0.5))+
  ggtitle("Swiss TPH Gender Categories in Citaion (with self-citation)")+
  xlab("Gender Category")+
  ylab("Frequency")+
  #theme(plot.title = element_text(size = 10))
  geom_text(aes(label=Frequency),vjust=-0.3,size=3)+
  theme(axis.text.x = element_text(size=8,face="bold"),
        plot.title = element_text(face = "bold"))

swisstph.self.cited #section 3.4.3.1

###6.1.3.  Result ----
swisstph.cited
swisstph.self.cited

###6.1.4. Swiss TPH Temporal Change w/o self-citation (not shown)----
View(gender.tph.df)
gender.tph.df=na.omit(gender.tph.df)

cited.tph.timedata=NULL
for(i in unique(gender.tph.df$Year)){
  sub=gender.tph.df[gender.tph.df$Year==i,]
  all=sum(sub$MM)+sum(sub$WM)+sum(sub$MW)+sum(sub$WW)
  cited.tph.timedata.i=data.frame(Year=rep(i,4),Gender=c("MM","WM","MW","WW"),
                                  Prop=c(sum(sub$MM)/all,
                                         sum(sub$WM)/all,
                                         sum(sub$MW)/all,
                                         sum(sub$WW)/all),
                                  Color=c("darkslateblue","darkslategray4",
                                          "lightcyan3","lightsalmon3"))
  cited.tph.timedata=rbind(cited.tph.timedata,cited.tph.timedata.i)
}
cited.tph.timedata$Color=as.character(cited.tph.timedata$Color)
cited.tph.timedata=na.omit(cited.tph.timedata)
f1plot(cited.tph.timedata,"Swiss TPH Citation by Year w/o Self-citation")# not shown in paper


###6.1.5 Swiss TPH Temporal Change w/ self-citation (not shown)----
View(self.gender.tph.df)
self.gender.tph.df=na.omit(self.gender.tph.df)

self.cited.timedata=NULL
for(i in unique(self.gender.tph.df$Year)){
  sub=self.gender.tph.df[self.gender.tph.df$Year==i,]
  self.cited.timedata.i=data.frame(Year=rep(i,4),Gender=c("MM","WM","MW","WW"),
                                   Sum=c(sum(sub$MM),sum(sub$WM),sum(sub$MW),sum(sub$WW)),
                                   Color=c("darkslateblue","darkslategray4",
                                           "lightcyan3","lightsalmon3"))
  self.cited.timedata=rbind(self.cited.timedata,self.cited.timedata.i)
}
self.cited.timedata$Color=as.character(self.cited.timedata$Color)

p.ov.tph.self=ggplot(data=self.cited.timedata,aes(x=Year, y=Sum, fill=Color))+
  geom_area(alpha=0.9,size=.5,color="black")+
  theme_bw()+
  scale_x_continuous(limits=c(min(self.cited.timedata$Year),max(self.cited.timedata$Year)),
                     expand=c(0,0),breaks = seq(1972,2021, by = 10))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_identity()+
  ggtitle("Swiss TPH Citation by Year w/ Self-citation")

# not shown in paper

##6.2 Race----
###6.2.1 Citation Frequency: without self-citation ----
item.index.sa=as.data.frame(article.tph.data$CP)
item.index.sa$sa=article.tph.data$SA
colnames(item.index.sa)=c("index","sa")
item.index.sa=item.index.sa[!item.index.sa$index=="",]
item.index.sa=cbind(item.index.sa,cited.notself= NA)

for (i in 1:nrow(item.index.sa)){
  cited.index=as.numeric(strsplit(item.index.sa$index[i],",")[[1]])
  sa=as.numeric(strsplit(item.index.sa$sa[i],",")[[1]])
  cited.notself=as.numeric(cited.index[!(cited.index%in%sa)])
  item.index.sa$cited.notself[i]=paste(as.character(cited.notself), collapse=", ")
}
item.index.sa=item.index.sa[!item.index.sa$cited.notself=="",]

item.index.sa=item.index.sa %>% 
  separate_rows(cited.notself)

cited.article.sa=as.numeric(item.index.sa$cited.notself)

cited.article.sa=as.data.frame(cited.article.sa)
cited.article.sa=as.data.frame(table(cited.article.sa$cited.article.sa))
colnames(cited.article.sa)=c("Index","Feq")

cited.article.sa$AR=article.data$AR[cited.article.sa$Index]
cited.article.sa$la_region=article.data$la_region[cited.article.sa$Index]
cited.article.sa$fa_region=article.data$fa_region[cited.article.sa$Index]
cited.article.sa$fa_gender=article.data$fa_gender[cited.article.sa$Index]
cited.article.sa$la_gender=article.data$la_gender[cited.article.sa$Index]
cited.article.sa$AR2=article.data$AR_2[cited.article.sa$Index]

table(cited.article.sa$AR2)


###summary of variables 
#fa_gender
M=subset(cited.article.sa, cited.article.sa$fa_gender=="M" , 
         select = c("Feq","fa_gender"))
sum(M$Feq)

W=subset(cited.article.sa, cited.article.sa$fa_gender=="W" , 
         select = c("Feq","fa_gender"))

sum(W$Feq)

U=subset(cited.article.sa, cited.article.sa$fa_gender=="U" , 
         select = c("Feq","fa_gender"))
sum(U$Feq)

#la_gender

M=subset(cited.article.sa, cited.article.sa$la_gender=="M" , 
         select = c("Feq","la_gender"))
sum(M$Feq)

W=subset(cited.article.sa, cited.article.sa$la_gender=="W" , 
         select = c("Feq","la_gender"))

sum(W$Feq)

U=subset(cited.article.sa, cited.article.sa$la_gender=="U" , 
         select = c("Feq","la_gender"))
sum(U$Feq)

#fa_region
EA=subset(cited.article.sa, cited.article.sa$fa_region =="Eastern Asia" , 
          select = c("Feq","fa_region"))
sum(EA$Feq)

EE=subset(cited.article.sa, cited.article.sa$fa_region =="Eastern Europe" , 
          select = c("Feq","fa_region"))
sum(EE$Feq)

An=subset(cited.article.sa, cited.article.sa$fa_region =="EnglishSpeaking Regions" , 
          select = c("Feq","fa_region"))
sum(An$Feq)

NAf=subset(cited.article.sa, cited.article.sa$fa_region =="Northern Africa" , 
           select = c("Feq","fa_region"))
sum(NAf$Feq)

NE=subset(cited.article.sa, cited.article.sa$fa_region =="Northern Europe" , 
          select = c("Feq","fa_region"))
sum(NE$Feq)

SEA=subset(cited.article.sa, cited.article.sa$fa_region =="South-eastern Asia" , 
           select = c("Feq","fa_region"))
sum(SEA$Feq)

SA=subset(cited.article.sa, cited.article.sa$fa_region =="Southern Asia" , 
          select = c("Feq","fa_region"))
sum(SA$Feq)

SEU=subset(cited.article.sa, cited.article.sa$fa_region =="Southern Europe" , 
           select = c("Feq","fa_region"))
sum(SEU$Feq)

SSA=subset(cited.article.sa, cited.article.sa$fa_region =="Sub-Saharan Africa" , 
           select = c("Feq","fa_region"))
sum(SSA$Feq)

UN=subset(cited.article.sa, cited.article.sa$fa_region =="Unknown" , 
          select = c("Feq","fa_region"))
sum(UN$Feq)

WE=subset(cited.article.sa, cited.article.sa$fa_region =="Wesrtern Europe" , 
          select = c("Feq","fa_region"))
sum(WE$Feq)


WA=subset(cited.article.sa, cited.article.sa$fa_region =="Western Asia" , 
          select = c("Feq","fa_region"))
sum(WA$Feq)

#la_region
EA=subset(cited.article.sa, cited.article.sa$la_region =="Eastern Asia" , 
          select = c("Feq","la_region"))
sum(EA$Feq)

EE=subset(cited.article.sa, cited.article.sa$la_region =="Eastern Europe" , 
          select = c("Feq","la_region"))
sum(EE$Feq)

An=subset(cited.article.sa, cited.article.sa$la_region =="EnglishSpeaking Regions" , 
          select = c("Feq","la_region"))
sum(An$Feq)

NAf=subset(cited.article.sa, cited.article.sa$la_region =="Northern Africa" , 
           select = c("Feq","la_region"))
sum(NAf$Feq)

NE=subset(cited.article.sa, cited.article.sa$la_region =="Northern Europe" , 
          select = c("Feq","la_region"))
sum(NE$Feq)

SEA=subset(cited.article.sa, cited.article.sa$la_region =="South-eastern Asia" , 
           select = c("Feq","la_region"))
sum(SEA$Feq)

SA=subset(cited.article.sa, cited.article.sa$la_region =="Southern Asia" , 
          select = c("Feq","la_region"))
sum(SA$Feq)

SEU=subset(cited.article.sa, cited.article.sa$la_region =="Southern Europe" , 
           select = c("Feq","la_region"))
sum(SEU$Feq)

SSA=subset(cited.article.sa, cited.article.sa$la_region =="Sub-Saharan Africa" , 
           select = c("Feq","la_region"))
sum(SSA$Feq)

UN=subset(cited.article.sa, cited.article.sa$la_region =="Unknown" , 
          select = c("Feq","la_region"))
sum(UN$Feq)

WE=subset(cited.article.sa, cited.article.sa$la_region =="Wesrtern Europe" , 
          select = c("Feq","la_region"))
sum(WE$Feq)


WA=subset(cited.article.sa, cited.article.sa$la_region =="Western Asia" , 
          select = c("Feq","la_region"))
sum(WA$Feq)

#region-pair

AfAf=subset(cited.article.sa, cited.article.sa$AR2 =="Africa+Africa" , 
            select = c("Feq","AR2"))
sum(AfAf$Feq)

AfAn=subset(cited.article.sa, cited.article.sa$AR2 =="Africa+Anglophone" , 
            select = c("Feq","AR2"))
sum(AfAn$Feq)

AfAs=subset(cited.article.sa, cited.article.sa$AR2 =="Africa+Asia" , 
            select = c("Feq","AR2"))
sum(AfAs$Feq)

AfEu=subset(cited.article.sa, cited.article.sa$AR2 =="Africa+EU" , 
            select = c("Feq","AR2"))
sum(AfEu$Feq)

AnAf=subset(cited.article.sa, cited.article.sa$AR2 =="Anglophone+Africa" , 
            select = c("Feq","AR2"))
sum(AnAf$Feq)

AnAn=subset(cited.article.sa, cited.article.sa$AR2 =="Anglophone+Anglophone" , 
            select = c("Feq","AR2"))
sum(AnAn$Feq)

AnAs=subset(cited.article.sa, cited.article.sa$AR2 =="Anglophone+Asia" , 
            select = c("Feq","AR2"))
sum(AnAs$Feq)

AnEu=subset(cited.article.sa, cited.article.sa$AR2 =="Anglophone+EU" , 
            select = c("Feq","AR2"))
sum(AnEu$Feq)

AsAf=subset(cited.article.sa, cited.article.sa$AR2 =="Asia+Africa" , 
            select = c("Feq","AR2"))
sum(AsAf$Feq)

AsAn=subset(cited.article.sa, cited.article.sa$AR2 =="Asia+Anglophone" , 
            select = c("Feq","AR2"))
sum(AsAn$Feq)

AsAs=subset(cited.article.sa, cited.article.sa$AR2 =="Asia+Asia" , 
            select = c("Feq","AR2"))
sum(AsAs$Feq)

AsEu=subset(cited.article.sa, cited.article.sa$AR2 =="Asia+EU" , 
            select = c("Feq","AR2"))
sum(AsEu$Feq)

EuAf=subset(cited.article.sa, cited.article.sa$AR2 =="EU+Africa" , 
            select = c("Feq","AR2"))
sum(EuAf$Feq)

EuAn=subset(cited.article.sa, cited.article.sa$AR2 =="EU+Anglophone" , 
            select = c("Feq","AR2"))
sum(EuAn$Feq)

EuAs=subset(cited.article.sa, cited.article.sa$AR2 =="EU+Asia" , 
            select = c("Feq","AR2"))
sum(EuAs$Feq)

EuEu=subset(cited.article.sa, cited.article.sa$AR2 =="EU+EU" , 
            select = c("Feq","AR2"))
sum(EuEu$Feq)

Unknown=subset(cited.article.sa, cited.article.sa$AR2 =="Unknown" , 
               select = c("Feq","AR2"))
sum(Unknown$Feq)










# Exclude "Unknown"
for(i in 1:nrow(cited.article.sa)){
  if(str_detect(cited.article.sa$AR[i], "Unknown")){
    cited.article.sa$exclusion[i]= 1 #exclude
  }else{
    cited.article.sa$exclusion[i]= 0 #include
  }
}
#### 1. Region Categories ----
#### 1. Eastern Asia 
for(i in 1:nrow(cited.article.sa)){
  if(str_detect(cited.article.sa$AR[i], "Eastern Asia")){
    cited.article.sa$EasternAsia[i]= 1
  }else{
    cited.article.sa$EasternAsia[i]= 0
  }
}

#### 2. Eastern Europe 
for(i in 1:nrow(cited.article.sa)){
  if(str_detect(cited.article.sa$AR[i], "Eastern Europe")){
    cited.article.sa$EasternEurope[i]= 1
  }else{
    cited.article.sa$EasternEurope[i]= 0
  }
}

#### 3. EnglishSpeaking Regions
for(i in 1:nrow(cited.article.sa)){
  if(str_detect(cited.article.sa$AR[i], "EnglishSpeaking Regions")){
    cited.article.sa$EnglishSpeakingRegions[i]= 1
  }else{
    cited.article.sa$EnglishSpeakingRegions[i]= 0
  }
}
#### 4. Northern Africa
for(i in 1:nrow(cited.article.sa)){
  if(str_detect(cited.article.sa$AR[i], "Northern Africa")){
    cited.article.sa$NorthernAfrica[i]= 1
  }else{
    cited.article.sa$NorthernAfrica[i]= 0
  }
}

#### 5. Northern Europe
for(i in 1:nrow(cited.article.sa)){
  if(str_detect(cited.article.sa$AR[i], "Northern Europe")){
    cited.article.sa$NorthernEurope[i]= 1
  }else{
    cited.article.sa$NorthernEurope[i]= 0
  }
}
#### 6. South-eastern Asia
for(i in 1:nrow(cited.article.sa)){
  if(str_detect(cited.article.sa$AR[i], "South-eastern Asia")){
    cited.article.sa$SE.Asia[i]= 1
  }else{
    cited.article.sa$SE.Asia[i]= 0
  }
}

#### 7. Southern Asia
for(i in 1:nrow(cited.article.sa)){
  if(str_detect(cited.article.sa$AR[i], "Southern Asia")){
    cited.article.sa$SouthernAsia[i]= 1
  }else{
    cited.article.sa$SouthernAsia[i]= 0
  }
}
#### 8. Southern Europe
for(i in 1:nrow(cited.article.sa)){
  if(str_detect(cited.article.sa$AR[i], "Southern Europe")){
    cited.article.sa$SouthernEurope[i]= 1
  }else{
    cited.article.sa$SouthernEurope[i]= 0
  }
}


#### 9. Sub-Saharan Africa
for(i in 1:nrow(cited.article.sa)){
  if(str_detect(cited.article.sa$AR[i], "Sub-Saharan Africa")){
    cited.article.sa$SSA[i]= 1
  }else{
    cited.article.sa$SSA[i]= 0
  }
}

#### 10. Western Europe

for(i in 1:nrow(cited.article.sa)){
  if(str_detect(cited.article.sa$AR[i], "Wesrtern Europe")){
    cited.article.sa$WesternEurope[i]= 1
  }else{
    cited.article.sa$WesternEurope[i]= 0
  }
}

#### 11. Western Asia
for(i in 1:nrow(cited.article.sa)){
  if(str_detect(cited.article.sa$AR[i], "Western Asia")){
    cited.article.sa$WesternAsia[i]= 1
  }else{
    cited.article.sa$WesternAsia[i]= 0
  }
}
#### 2. Data Summary- with unknown----
citation.sum.sa=data.frame(matrix(nrow=11,ncol=2))
colnames(citation.sum.sa)= c("Region","Freq")
citation.sum.sa$Region=c("E.Asia","E.Europe","E.SpeakingRegions","N.Africa",
                         "N.Europe","S.E.Asia","S.Asia","S.Europe","SSA",
                         "W.Europe","W.Asia")

citation.sum.sa$Fill=c("darkslateblue","darkslategray4","lightsalmon3",
                       "lightcyan3","darkslategray4","darkslateblue",
                       "darkslateblue","darkslategray4","lightcyan3",
                       "darkslategray4","darkslateblue")

citation.sum.sa[1,2]=sum(cited.article.sa[which(cited.article.sa$EasternAsia== 1),2])
citation.sum.sa[2,2]=sum(cited.article.sa[which(cited.article.sa$EasternEurope == 1),2])
citation.sum.sa[3,2]=sum(cited.article.sa[which(cited.article.sa$EnglishSpeakingRegions == 1),2])
citation.sum.sa[4,2]=sum(cited.article.sa[which(cited.article.sa$NorthernAfrica == 1),2])
citation.sum.sa[5,2]=sum(cited.article.sa[which(cited.article.sa$NorthernEurope== 1),2])
citation.sum.sa[6,2]=sum(cited.article.sa[which(cited.article.sa$SE.Asia == 1),2])
citation.sum.sa[7,2]=sum(cited.article.sa[which(cited.article.sa$SouthernAsia == 1),2])
citation.sum.sa[8,2]=sum(cited.article.sa[which(cited.article.sa$SouthernEurope == 1),2])
citation.sum.sa[9,2]=sum(cited.article.sa[which(cited.article.sa$SSA == 1),2])
citation.sum.sa[10,2]=sum(cited.article.sa[which(cited.article.sa$WesternEurope== 1),2])
citation.sum.sa[11,2]=sum(cited.article.sa[which(cited.article.sa$WesternAsia == 1),2])
#### 3. Data Visualization ----
R1=ggplot(data=citation.sum.sa,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill=citation.sum.sa$Fill) +
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Swiss TPH Race Categories in Citation without self-citation(incl.unknown)")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)+
  theme(axis.text.x = element_text(size=8,face="bold"),
      plot.title = element_text(face = "bold"))

####4. Data Summary- without unknown----
View(cited.article.sa)
sub.articles.sa=subset(cited.article.sa,cited.article.sa$exclusion==0)

citation.sum.sa.k=data.frame(matrix(nrow=11,ncol=2))
colnames(citation.sum.sa.k)= c("Region","Freq")
citation.sum.sa.k$Region=c("E.Asia","E.Europe","E.SpeakingRegions","N.Africa",
                           "N.Europe","S.E.Asia","S.Asia","S.Europe","SSA",
                           "W.Europe","W.Asia")

citation.sum.sa.k$Fill=c("darkslateblue","darkslategray4","lightsalmon3",
                         "lightcyan3","darkslategray4","darkslateblue",
                         "darkslateblue","darkslategray4","lightcyan3",
                         "darkslategray4","darkslateblue")

citation.sum.sa.k[1,2]=sum(sub.articles.sa[which(sub.articles.sa$EasternAsia== 1),2])
citation.sum.sa.k[2,2]=sum(sub.articles.sa[which(sub.articles.sa$EasternEurope == 1),2])
citation.sum.sa.k[3,2]=sum(sub.articles.sa[which(sub.articles.sa$EnglishSpeakingRegions == 1),2])
citation.sum.sa.k[4,2]=sum(sub.articles.sa[which(sub.articles.sa$NorthernAfrica == 1),2])
citation.sum.sa.k[5,2]=sum(sub.articles.sa[which(sub.articles.sa$NorthernEurope== 1),2])
citation.sum.sa.k[6,2]=sum(sub.articles.sa[which(sub.articles.sa$SE.Asia == 1),2])
citation.sum.sa.k[7,2]=sum(sub.articles.sa[which(sub.articles.sa$SouthernAsia == 1),2])
citation.sum.sa.k[8,2]=sum(sub.articles.sa[which(sub.articles.sa$SouthernEurope == 1),2])
citation.sum.sa.k[9,2]=sum(sub.articles.sa[which(sub.articles.sa$SSA == 1),2])
citation.sum.sa.k[10,2]=sum(sub.articles.sa[which(sub.articles.sa$WesternEurope== 1),2])
citation.sum.sa.k[11,2]=sum(sub.articles.sa[which(sub.articles.sa$WesternAsia == 1),2])

#### 5.  Data Visualization ----
R2=ggplot(data=citation.sum.sa.k,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill=citation.sum.sa.k$Fill) +
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Swiss TPH Race Categories in Citation without self-citation (excl.unknown)")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)+
  theme(axis.text.x = element_text(size=8,face="bold"),
        plot.title = element_text(face = "bold"))
#### 6. Result ----
R1/R2
###6.2.2 Citation Frequency: with self-citation ----
item.index=as.data.frame(article.tph.data$CP)
item.index=as.data.frame(item.index[!apply(item.index == "", 1, all), ] ) 
colnames(item.index)=c("index")

item.index=item.index %>% 
  separate_rows(index)

cited.article=as.data.frame(table(item.index$index))
colnames(cited.article)=c("Index","Feq")
cited.article$Index=as.numeric(cited.article$Index)
cited.article$AR=article.data$AR[cited.article$Index]
cited.article$AR_2=article.data$AR_2[cited.article$Index]


















# Exclude "Unknown"
for(i in 1:nrow(cited.article)){
  if(str_detect(cited.article$AR[i], "Unknown")){
    cited.article$exclusion[i]= 1 #exclude
  }else{
    cited.article$exclusion[i]= 0 #include
  }
}
#### 1. Region Categories ----
#### 1. Eastern Asia 
for(i in 1:nrow(cited.article)){
  if(str_detect(cited.article$AR[i], "Eastern Asia")){
    cited.article$EasternAsia[i]= 1
  }else{
    cited.article$EasternAsia[i]= 0
  }
}

#### 2. Eastern Europe 
for(i in 1:nrow(cited.article)){
  if(str_detect(cited.article$AR[i], "Eastern Europe")){
    cited.article$EasternEurope[i]= 1
  }else{
    cited.article$EasternEurope[i]= 0
  }
}
#### 3. EnglishSpeaking Regions
for(i in 1:nrow(cited.article)){
  if(str_detect(cited.article$AR[i], "EnglishSpeaking Regions")){
    cited.article$EnglishSpeakingRegions[i]= 1
  }else{
    cited.article$EnglishSpeakingRegions[i]= 0
  }
}
#### 4. Northern Africa
for(i in 1:nrow(cited.article)){
  if(str_detect(cited.article$AR[i], "Northern Africa")){
    cited.article$NorthernAfrica[i]= 1
  }else{
    cited.article$NorthernAfrica[i]= 0
  }
}

#### 5. Northern Europe
for(i in 1:nrow(cited.article)){
  if(str_detect(cited.article$AR[i], "Northern Europe")){
    cited.article$NorthernEurope[i]= 1
  }else{
    cited.article$NorthernEurope[i]= 0
  }
}
#### 6. South-eastern Asia
for(i in 1:nrow(cited.article)){
  if(str_detect(cited.article$AR[i], "South-eastern Asia")){
    cited.article$SE.Asia[i]= 1
  }else{
    cited.article$SE.Asia[i]= 0
  }
}

#### 7. Southern Asia
for(i in 1:nrow(cited.article)){
  if(str_detect(cited.article$AR[i], "Southern Asia")){
    cited.article$SouthernAsia[i]= 1
  }else{
    cited.article$SouthernAsia[i]= 0
  }
}
#### 8. Southern Europe
for(i in 1:nrow(cited.article)){
  if(str_detect(cited.article$AR[i], "Southern Europe")){
    cited.article$SouthernEurope[i]= 1
  }else{
    cited.article$SouthernEurope[i]= 0
  }
}


#### 9. Sub-Saharan Africa
for(i in 1:nrow(cited.article)){
  if(str_detect(cited.article$AR[i], "Sub-Saharan Africa")){
    cited.article$SSA[i]= 1
  }else{
    cited.article$SSA[i]= 0
  }
}

#### 10. Western Europe
for(i in 1:nrow(cited.article)){
  if(str_detect(cited.article$AR[i], "Wesrtern Europe")){
    cited.article$WesternEurope[i]= 1
  }else{
    cited.article$WesternEurope[i]= 0
  }
}

#### 11. Western Asia
for(i in 1:nrow(cited.article)){
  if(str_detect(cited.article$AR[i], "Western Asia")){
    cited.article$WesternAsia[i]= 1
  }else{
    cited.article$WesternAsia[i]= 0
  }
}
####2. Data Summary- with unknown----

sub.easternAsia=subset(cited.article,cited.article$EasternAsia==1)
nrow(sub.easternAsia)
citeno.eAsia=sum(sub.easternAsia$Feq)

sub.easternEurope=subset(cited.article,cited.article$EasternEurope ==1)
nrow(sub.easternEurope)
citeno.eaternEurope=sum(sub.easternEurope$Feq)

sub.EnglishSpeakingRegion=subset(cited.article,cited.article$EnglishSpeakingRegions ==1)
nrow(sub.EnglishSpeakingRegion)
citeno.EnglishSpeakingRegion=sum(sub.EnglishSpeakingRegion$Feq)

sub.northernAfrica=subset(cited.article,cited.article$NorthernAfrica ==1)
nrow(sub.northernAfrica)
citeno.northernAfrica=sum(sub.northernAfrica$Feq)

sub.southeasternAsia=subset(cited.article,cited.article$SE.Asia ==1)
nrow(sub.southeasternAsia)
citeno.southeasternAsia=sum(sub.southeasternAsia$Feq)

sub.southernAsia=subset(cited.article,cited.article$SouthernAsia ==1)
nrow(sub.southernAsia)
citeno.southernAsia=sum(sub.southernAsia$Feq)

sub.southernEurope=subset(cited.article,cited.article$SouthernEurope ==1)
nrow(sub.southernEurope)
citeno.southernEurope=sum(sub.southernEurope$Feq)
citeno.southernEurope

sub.SSA=subset(cited.article,cited.article$SSA ==1)
nrow(sub.SSA)
citeno.SSA=sum(sub.SSA$Feq)
citeno.SSA

sub.WesternEurope=subset(cited.article,cited.article$WesternEurope ==1)
nrow(sub.WesternEurope)
citeno.WesternEurope=sum(sub.WesternEurope$Feq)
citeno.WesternEurope

sub.WesternAsia=subset(cited.article,cited.article$WesternAsia ==1)
nrow(sub.WesternAsia)
citeno.WesternAsia=sum(sub.WesternAsia$Feq)
citeno.WesternAsia

sub.NorthernEurope=subset(cited.article,cited.article$NorthernEurope ==1)
nrow(sub.NorthernEurope)
citeno.NorthernEurope=sum(sub.NorthernEurope$Feq)
citeno.NorthernEurope


citation.sum=data.frame(matrix(nrow=11,ncol=2))
colnames(citation.sum)= c("Region","Freq")
citation.sum$Region=c("E.Asia","E.Europe","EnglishSpeakingRegions","N.Africa",
                      "N.Europe","S.E.Asia","S.Asia","S.Europe","SSA",
                      "W.Europe","W.Asia")

citation.sum$Freq=c(citeno.eAsia,citeno.eaternEurope,citeno.EnglishSpeakingRegion,
                    citeno.northernAfrica,citeno.NorthernEurope,citeno.southeasternAsia,
                    citeno.southernAsia,citeno.southernEurope,citeno.SSA,
                    citeno.WesternEurope,citeno.WesternAsia)  

citation.sum$Fill=c("darkslateblue","darkslategray4","lightsalmon3",
                    "lightcyan3","darkslategray4","darkslateblue",
                    "darkslateblue","darkslategray4","lightcyan3",
                    "darkslategray4","darkslateblue")
citation.sum=citation.sum[order(citation.sum$Freq),]
#### 3. Data Visualization ----
R3=ggplot(data=citation.sum,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill=citation.sum$Fill) +
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Swiss TPH Race Categories in Citation with self-citation (incl.unknown)")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)+
  theme(axis.text.x = element_text(size=8,face="bold"),
      plot.title = element_text(face = "bold"))

#### 4. Data Summary- without unknown----

sub.articles=subset(cited.article,cited.article$exclusion==0)

citation.sum.known=data.frame(matrix(nrow=11,ncol=2))
colnames(citation.sum.known)= c("Region","Freq")
citation.sum.known$Region=c("E.Asia","E.Europe","EnglishSpeakingRegions","N.Africa",
                            "N.Europe","S.E.Asia","S.Asia","S.Europe","SSA",
                            "W.Europe","W.Asia")

citation.sum.known$Fill=c("darkslateblue","darkslategray4","lightsalmon3",
                          "lightcyan3","darkslategray4","darkslateblue",
                          "darkslateblue","darkslategray4","lightcyan3",
                          "darkslategray4","darkslateblue")

k.sub.easternAsia=subset(sub.articles,sub.articles$EasternAsia==1)
nrow(k.sub.easternAsia)
k.citeno.eAsia=sum(k.sub.easternAsia$Feq)

k.sub.easternEurope=subset(sub.articles,sub.articles$EasternEurope ==1)
nrow(k.sub.easternEurope)
k.citeno.eaternEurope=sum(k.sub.easternEurope$Feq)

k.sub.EnglishSpeakingRegion=subset(sub.articles,sub.articles$EnglishSpeakingRegions ==1)
nrow(k.sub.EnglishSpeakingRegion)
k.citeno.EnglishSpeakingRegion=sum(k.sub.EnglishSpeakingRegion$Feq)

k.sub.northernAfrica=subset(sub.articles,sub.articles$NorthernAfrica ==1)
nrow(k.sub.northernAfrica)
k.citeno.northernAfrica=sum(k.sub.northernAfrica$Feq)

k.sub.southeasternAsia=subset(sub.articles,sub.articles$SE.Asia ==1)
nrow(k.sub.southeasternAsia)
k.citeno.southeasternAsia=sum(k.sub.southeasternAsia$Feq)

k.sub.southernAsia=subset(sub.articles,sub.articles$SouthernAsia ==1)
nrow(k.sub.southernAsia)
k.citeno.southernAsia=sum(k.sub.southernAsia$Feq)

k.sub.southernEurope=subset(sub.articles,sub.articles$SouthernEurope ==1)
nrow(k.sub.southernEurope)
k.citeno.southernEurope=sum(k.sub.southernEurope$Feq)
k.citeno.southernEurope

k.sub.SSA=subset(sub.articles,sub.articles$SSA ==1)
nrow(k.sub.SSA)
k.citeno.SSA=sum(k.sub.SSA$Feq)
k.citeno.SSA

k.sub.WesternEurope=subset(sub.articles,sub.articles$WesternEurope ==1)
nrow(k.sub.WesternEurope)
k.citeno.WesternEurope=sum(k.sub.WesternEurope$Feq)
k.citeno.WesternEurope

k.sub.WesternAsia=subset(sub.articles,sub.articles$WesternAsia ==1)
nrow(k.sub.WesternAsia)
k.citeno.WesternAsia=sum(k.sub.WesternAsia$Feq)
k.citeno.WesternAsia

k.sub.NorthernEurope=subset(sub.articles,sub.articles$N.Europe ==1)
nrow(k.sub.NorthernEurope)
k.citeno.NorthernEurope=sum(k.sub.NorthernEurope$Feq)
k.citeno.NorthernEurope

citation.sum.known$Freq=c(k.citeno.eAsia,k.citeno.eaternEurope,k.citeno.EnglishSpeakingRegion,
                          k.citeno.northernAfrica,k.citeno.NorthernEurope,k.citeno.southeasternAsia,
                          k.citeno.southernAsia,k.citeno.southernEurope,k.citeno.SSA,
                          k.citeno.WesternEurope,k.citeno.WesternAsia)  

citation.sum.known=citation.sum.known[order(citation.sum.known$Freq),]

#### 5. Data Visualization ----
R4=ggplot(data=citation.sum.known,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill=citation.sum.known$Fill) +
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Swiss TPH Race Categories in Citation with self-citation (excl.unknown)")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)+
  theme(axis.text.x = element_text(size=8,face="bold"),
        plot.title = element_text(face = "bold"))
R3/R4 

#7. Citation Analysis----


