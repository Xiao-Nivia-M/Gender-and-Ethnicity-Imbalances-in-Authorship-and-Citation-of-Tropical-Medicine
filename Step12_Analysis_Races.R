# 1. Library Importation----
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(patchwork)

# 2. Data Importation----
load("df7_articledata_withraces.RData")

#3. Citation Frequency: with self-citation ----
item.index=as.data.frame(article.data$CP)
item.index=as.data.frame(item.index[!apply(item.index == "", 1, all), ] ) 
colnames(item.index)=c("index")

item.index=item.index %>% 
  separate_rows(index)

cited.article=as.data.frame(table(item.index$index))
colnames(cited.article)=c("Index","Feq")
cited.article$Index=as.numeric(cited.article$Index)
cited.article$AR=article.data$AR[cited.article$Index]

# Exclude "Unknown"
for(i in 1:nrow(cited.article)){
if(str_detect(cited.article$AR[i], "Unknown")){
  cited.article$exclusion[i]= 1 #exclude
}else{
  cited.article$exclusion[i]= 0 #include
}
}


### 3.1 Region Categories ----
# 1st or last author is 
# 1. Eastern Asia 
# 2. Eastern Europe
# 3. EnglishSpeaking Regions
# 4. Northern Africa
# 5. Northern Europe
# 6. South-eastern Asia
# 7. Southern Asia
# 8. Southern Europe
# 9. Sub-Saharan Africa
# 10. Western Europe
# 11. Western Asia
# -1. Unknown

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

save(cited.article,file = "df12_citedarticle_region.Rdata")

### 3.2.1 Data Summary- with unknown----

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

### 3.2.2 Data Visualization ----
#Asia
subset.Asia=citation.sum[str_detect(citation.sum$Region,"Asia"),]
P.A=ggplot(data=subset.Asia,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="darkslateblue")+
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Asia")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)


#Europe
subset.Europe=citation.sum[str_detect(citation.sum$Region,"Europe"),]
P.E=ggplot(data=subset.Europe,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="darkslategray4") +
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Europe")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

#Africa
subset.Africa=citation.sum[c(3,6),]
P.AF=ggplot(data=subset.Africa,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="lightcyan3") +
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Africa")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

#Overall
P.all=ggplot(data=citation.sum,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill=citation.sum$Fill) +
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Overall")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)
  

Figure1=ggarrange(P.A, P.E, P.AF,P.all,
          labels = c("A", "B", "C","D"),
          ncol = 2, nrow = 2)+
  plot_annotation(title = "Tropical Medicine Citation Frequency by first/last Author Origin(w/ unknown)",
    theme = theme(plot.title = element_text(hjust = 0.5)))
### 3.3.1 Data Summary- without unknown----
View(cited.article)
sub.articles=subset(cited.article,cited.article$exclusion==0)

citation.sum.known=data.frame(matrix(nrow=11,ncol=2))
colnames(citation.sum.known)= c("Region","Freq")
citation.sum.known$Region=c("E.Asia","E.Europe","EnglishSpeakingRegions","N.Africa",
                            "N.Europe","S.E.Asia","S.Asia","S.Europe","SSA",
                            "W.Europe","W.Asia")

#citation.sum.known$Freq=c(citeno.eAsia,citeno.eaternEurope,citeno.EnglishSpeakingRegion,
                          #citeno.northernAfrica,citeno.NorthernEurope,citeno.southeasternAsia,
                          #citeno.southernAsia,citeno.southernEurope,citeno.SSA,
                          #citeno.WesternEurope,citeno.WesternAsia)  

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
### 3.3.2 Data Visualization ----
#Asia
subset.Asia.k=citation.sum.known[str_detect(citation.sum.known$Region,"Asia"),]
P.A.k=ggplot(data=subset.Asia.k,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="darkslateblue")+
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Asia")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)
#Europe
subset.Europe.k=citation.sum.known[str_detect(citation.sum.known$Region,"Europe"),]
P.E.k=ggplot(data=subset.Europe.k,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="darkslategray4") +
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Europe")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

#Africa
subset.Africa.k=citation.sum.known[c(3,7),]
P.AF.k=ggplot(data=subset.Africa.k,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="lightcyan3") +
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Africa")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

#Overall
P.all.k=ggplot(data=citation.sum.known,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill=citation.sum.known$Fill) +
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Overall")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

Figure2=ggarrange(P.A.k, P.E.k, P.AF.k,P.all.k,
                  labels = c("A", "B", "C","D"),
                  ncol = 2, nrow = 2)+
  plot_annotation(title = "Tropical Medicine Citation Frequency by first/last Author Origin (w/o unknown)",
                  theme = theme(plot.title = element_text(hjust = 0.5)))
## 3.4 Result----
#### 3.4.1 Result1 ----
Figure1
#### 3.4.2 Result2 ----
Figure2

#4. Citation Frequency: without self-citation ----
item.index.sa=as.data.frame(article.data$CP)
item.index.sa$sa=article.data$SA
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

### 4.1 Region Categories ----
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

### 4.2.1 Data Summary- with unknown----
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

### 4.2.2 Data Visualization ----
#Asia
subset.Asia.sa=citation.sum.sa[str_detect(citation.sum.sa$Region,"Asia"),]
P.A.SA=ggplot(data=subset.Asia.sa,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="darkslateblue")+
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Asia")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)


#Europe
subset.Europe.sa=citation.sum.sa[str_detect(citation.sum.sa$Region,"Europe"),]
P.E.SA=ggplot(data=subset.Europe.sa,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="darkslategray4") +
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Europe")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

#Africa
subset.Africa.sa=citation.sum.sa[c(4,9),]
P.AF.SA=ggplot(data=subset.Africa.sa,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="lightcyan3") +
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Africa")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

#Overall
P.all.sa=ggplot(data=citation.sum.sa,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill=citation.sum.sa$Fill) +
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Overall")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

Figure3=ggarrange(P.A.SA, P.E.SA, P.AF.SA,P.all.sa,
                  labels = c("A", "B", "C","D"),
                  ncol = 2, nrow = 2)+
  plot_annotation(title = "Tropical Medicine Citation Frequency by first/last Author Origin (w/ unknown-self-citation excluded)",
                  theme = theme(plot.title = element_text(hjust = 0.5)))

### 4.3.1 Data Summary- without unknown----
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

### 4.3.2 Data Visualization ----
#Asia
subset.Asia.k.sa=citation.sum.sa.k[str_detect(citation.sum.sa.k$Region,"Asia"),]
P.A.SA.K=ggplot(data=subset.Asia.k.sa,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="darkslateblue")+
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Asia")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

#Europe
subset.Europe.k.sa=citation.sum.sa.k[str_detect(citation.sum.sa.k$Region,"Europe"),]
P.E.SA.K=ggplot(data=subset.Europe.k.sa,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="darkslategray4") +
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Europe")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

#Africa
subset.Africa.k.sa=citation.sum.sa.k[c(4,9),]
P.AF.SA.K=ggplot(data=subset.Africa.k.sa,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="lightcyan3") +
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Africa")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

#Overall
P.all.SA.K=ggplot(data=citation.sum.sa.k,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill=citation.sum.sa.k$Fill) +
  xlab("Region")+
  ylab("Citation Times")+
  ggtitle("Overall")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

Figure4=ggarrange(P.A.SA.K, P.E.SA.K, P.AF.SA.K,P.all.SA.K,
                  labels = c("A", "B", "C","D"),
                  ncol = 2, nrow = 2)+
  plot_annotation(title = "Tropical Medicine Citation Frequency by first/last Author Origin (w/o unknown & self-citation excluded)",
                  theme = theme(plot.title = element_text(hjust = 0.5)))

## 4.4 Result----
#### 4.4.1 Result1 ----
Figure3
#### 4.4.2 Result2 ----
Figure4

# 5. Authorship----
View(article.data)
article.data$AR
authorship=data.frame(matrix(nrow=nrow(article.data),ncol=11))
colnames(authorship)=c("E.Asia","E.Europe","E.SpeakingRegions","N.Africa",
                       "N.Europe","S.E.Asia","S.Asia","S.Europe","SSA",
                       "W.Europe","W.Asia")
authorship$AR=article.data$AR
authorship$PY=article.data$PY

# Exclude "Unknown"
for(i in 1:nrow(authorship)){
  if(str_detect(authorship$AR[i], "Unknown")){
    authorship$exclusion[i]= 1 #exclude
  }else{
    authorship$exclusion[i]= 0 #include
  }
}

###****Added plot: Overview*****----
O1=as.data.frame(table(authorship$exclusion))
O1$Category[1]="w/o unknown"
O1$Category[2]="w/ unknown"

ggplot(data=O1, aes(x=Category,y=Freq))+
  geom_bar(stat = "identity",fill=c("steelblue","gray"))+
  xlab("Origin Region Category")+
  ylab("Frequency")+
  ggtitle("Origin Region Assignment Overview")+
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
## 5.1 with unknown:summary----
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

## 5.2 with unknown:visualization----
#Asia
Asia=summary[str_detect(summary$Region,"Asia"),]
A.A=ggplot(data=Asia,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="darkslateblue")+
  xlab("Region")+
  ylab("Paper No.")+
  ggtitle("Asia")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

#Europe
Europe=summary[str_detect(summary$Region,"Europe"),]
A.E=ggplot(data=Europe,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="darkslategray4") +
  xlab("Region")+
  ylab("Paper No.")+
  ggtitle("Europe")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

#Africa
Africa=summary[c(4,9),]
A.AF=ggplot(data=Africa,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="lightcyan3") +
  xlab("Region")+
  ylab("Paper No.")+
  ggtitle("Africa")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

#Overall
A.All=ggplot(data=summary,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill=summary$Fill) +
  xlab("Region")+
  ylab("Paper No.")+
  ggtitle("Overall")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

Figure5=ggarrange(A.A,A.E,A.AF,A.All,
                  labels = c("A", "B", "C","D"),
                  ncol = 2, nrow = 2)+
  plot_annotation(title = "Author Origin Region Assignment (w/ unknown)",
                  theme = theme(plot.title = element_text(hjust = 0.5)))

##"Tropical Medicine Authorship by first/last Author Origin (w/ unknown)"

## 5.3 without unknown:summary----
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


## 5.4 without unknown:visualization----
#Asia
Asia.k=summary.k[str_detect(summary.k$Region,"Asia"),]
A.A.K=ggplot(data=Asia.k,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="darkslateblue")+
  xlab("Region")+
  ylab("Paper No.")+
  ggtitle("Asia")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

#Europe
Europe.k=summary.k[str_detect(summary.k$Region,"Europe"),]
A.E.K=ggplot(data=Europe.k,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="darkslategray4") +
  xlab("Region")+
  ylab("Paper No.")+
  ggtitle("Europe")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

#Africa
Africa.k=summary.k[c(4,9),]
A.AF.K=ggplot(data=Africa.k,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="lightcyan3") +
  xlab("Region")+
  ylab("Paper No.")+
  ggtitle("Africa")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

#Overall
A.All.K=ggplot(data=summary.k,aes(x=reorder(Region,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill=summary.k$Fill) +
  xlab("Region")+
  ylab("Paper No.")+
  ggtitle("Overall")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)


Figure6=ggarrange(A.A.K,A.E.K,A.AF.K,A.All.K,
                  labels = c("A", "B", "C","D"),
                  ncol = 2, nrow = 2)+
  plot_annotation(title = "Author Origin Region Assignment (w/o unknown)",
                  theme = theme(plot.title = element_text(hjust = 0.5)))

#Tropical Medicine Authorship by first/last Author Origin (w/o unknown)

## 5.5.Without unknown: Temporal change in Authorship-----
authorship.k

E.Asia=authorship.k%>%
  group_by(PY)%>%
  summarise(E.Asia=sum(E.Asia))
E.Asia=na.omit(E.Asia)

E.Europe=authorship.k%>%
  group_by(PY)%>%
  summarise(E.Europe=sum(E.Europe))
E.Europe=na.omit(E.Europe)

E.SpeakingRegions=authorship.k%>%
  group_by(PY)%>%
  summarise(E.SpeakingRegions=sum(E.SpeakingRegions))
E.SpeakingRegions=na.omit(E.SpeakingRegions)

N.Africa =authorship.k%>%
  group_by(PY)%>%
  summarise(N.Africa =sum(N.Africa ))
N.Africa=na.omit(N.Africa)

N.Europe=authorship.k%>%
  group_by(PY)%>%
  summarise(N.Europe=sum(N.Europe))
N.Europe=na.omit(N.Europe)

S.Asia=authorship.k%>%
  group_by(PY)%>%
  summarise(S.Asia=sum(S.Asia))
S.Asia=na.omit(S.Asia)

  
S.E.Asia=authorship.k%>%
  group_by(PY)%>%
  summarise(S.E.Asia=sum(S.E.Asia))
S.E.Asia=na.omit(S.E.Asia)

S.Europe=authorship.k%>%
  group_by(PY)%>%
  summarise(S.Europe=sum(S.Europe))
S.Europe=na.omit(S.Europe)

SSA=authorship.k%>%
  group_by(PY)%>%
  summarise(SSA=sum(SSA))
SSA=na.omit(SSA)

W.Asia=authorship.k%>%
  group_by(PY)%>%
  summarise(W.Asia=sum(W.Asia))
W.Asia=na.omit(W.Asia)

W.Europe=authorship.k%>%
  group_by(PY)%>%
  summarise(W.Europe=sum(W.Europe))
W.Europe=na.omit(W.Europe)

sum=E.Asia
sum$E.Europe=E.Europe$E.Europe
sum$E.SpeakingRegions=E.SpeakingRegions$E.SpeakingRegions
sum$N.Africa=N.Africa$N.Africa
sum$N.Europe=N.Europe$N.Europe
sum$S.E.Asia=S.E.Asia$S.E.Asia
sum$S.Asia=S.Asia$S.Asia
sum$S.Europe=S.Europe$S.Europe
sum$SSA=SSA$SSA
sum$W.Europe=W.Europe$W.Europe
sum$W.Asia=W.Asia$W.Asia

sum$Asia=sum$E.Asia+sum$S.E.Asia+sum$S.Asia+sum$W.Asia
sum$Europe=sum$E.Europe+sum$N.Europe+sum$W.Europe+sum$S.Europe
sum$Afria=sum$N.Africa+sum$SSA

author.region=sum %>%
  select("PY","Asia","Europe","Afria","E.SpeakingRegions")
colnames(author.region)[4]="Africa"

t=NULL
for (i in unique(author.region$PY)){
  subregion=author.region[author.region$PY==i,]
  t.i=data.frame(matrix(nrow=4,ncol=4))
  colnames(t.i)=c("Year","Region","Prop","Color")
  t.i$Year=c(i,i,i,i)
  t.i$Region=c("Anglophone Regions","Europe","Asia","Africa")
  t.i$Color=c("#994455","#997700","#EE99AA","#6699CC")
  all=subregion$Asia+subregion$Europe+subregion$Africa+subregion$E.SpeakingRegions
  t.i$Prop=c((subregion$E.SpeakingRegions)/all,(subregion$Europe)/all,(subregion$Asia)/all,(subregion$Africa)/all)
  t=rbind(t,t.i)
}


t$Color=as.character(t$Color)
t=t[-c(309:312),]

f1plot(t,"Ethnicities of Authorship in Tropical Medicine")
ggsave("~/switchdrive/DataAnalysis /Github_R_Link_Test/Outcome/Plot/Ethnicities of Authorship in Tropical Medicine.jpg",
       width = 10,
       height = 8)

  
##5.6 With unknown: Temporal change in Authorship-----
authorship

E.Asia=authorship%>%
  group_by(PY)%>%
  summarise(E.Asia=sum(E.Asia))
E.Asia=na.omit(E.Asia)

E.Europe=authorship%>%
  group_by(PY)%>%
  summarise(E.Europe=sum(E.Europe))
E.Europe=na.omit(E.Europe)

E.SpeakingRegions=authorship%>%
  group_by(PY)%>%
  summarise(E.SpeakingRegions=sum(E.SpeakingRegions))
E.SpeakingRegions=na.omit(E.SpeakingRegions)

N.Africa =authorship%>%
  group_by(PY)%>%
  summarise(N.Africa =sum(N.Africa ))
N.Africa=na.omit(N.Africa)

N.Europe=authorship%>%
  group_by(PY)%>%
  summarise(N.Europe=sum(N.Europe))
N.Europe=na.omit(N.Europe)

S.Asia=authorship%>%
  group_by(PY)%>%
  summarise(S.Asia=sum(S.Asia))
S.Asia=na.omit(S.Asia)


S.E.Asia=authorship%>%
  group_by(PY)%>%
  summarise(S.E.Asia=sum(S.E.Asia))
S.E.Asia=na.omit(S.E.Asia)

S.Europe=authorship%>%
  group_by(PY)%>%
  summarise(S.Europe=sum(S.Europe))
S.Europe=na.omit(S.Europe)

SSA=authorship%>%
  group_by(PY)%>%
  summarise(SSA=sum(SSA))
SSA=na.omit(SSA)

W.Asia=authorship%>%
  group_by(PY)%>%
  summarise(W.Asia=sum(W.Asia))
W.Asia=na.omit(W.Asia)

W.Europe=authorship%>%
  group_by(PY)%>%
  summarise(W.Europe=sum(W.Europe))
W.Europe=na.omit(W.Europe)

sum=E.Asia
sum$E.Europe=E.Europe$E.Europe
sum$E.SpeakingRegions=E.SpeakingRegions$E.SpeakingRegions
sum$N.Africa=N.Africa$N.Africa
sum$N.Europe=N.Europe$N.Europe
sum$S.E.Asia=S.E.Asia$S.E.Asia
sum$S.Asia=S.Asia$S.Asia
sum$S.Europe=S.Europe$S.Europe
sum$SSA=SSA$SSA
sum$W.Europe=W.Europe$W.Europe
sum$W.Asia=W.Asia$W.Asia

sum$Asia=sum$E.Asia+sum$S.E.Asia+sum$S.Asia+sum$W.Asia
sum$Europe=sum$E.Europe+sum$N.Europe+sum$W.Europe+sum$S.Europe
sum$Afria=sum$N.Africa+sum$SSA

author.region=sum %>%
  select("PY","Asia","Europe","Afria","E.SpeakingRegions")
colnames(author.region)[4]="Africa"

t2=NULL
for (i in unique(author.region$PY)){
  subregion=author.region[author.region$PY==i,]
  t2.i=data.frame(matrix(nrow=4,ncol=4))
  colnames(t2.i)=c("Year","Region","Prop","Color")
  t2.i$Year=c(i,i,i,i)
  t2.i$Region=c("E.SpeakingRegions","Europe","Asia","Africa")
  t2.i$Color=c("darkslateblue","darkslategray4",
              "lightcyan3","lightsalmon3")
  all=subregion$Asia+subregion$Europe+subregion$Africa+subregion$E.SpeakingRegions
  t2.i$Prop=c((subregion$E.SpeakingRegions)/all,(subregion$Europe)/all,(subregion$Asia)/all,(subregion$Africa)/all)
  t2=rbind(t2,t2.i)
}

t2$Color=as.character(t2$Color)
t2=t2[-c(309:312),]
n2=f1plot(t2,"Race:Overall Authorship by Year (w/ unknown)")



## 5.5 Result----
Figure5 #(paper: 3.2.2.	Authorship: First/Last Author Origin Region Assignment Result )
Figure6 #(paper: 3.2.2.	Authorship: First/Last Author Origin Region Assignment Result )

save(Figure1,Figure2,Figure3,Figure4,Figure5,Figure6,file ="df12_race_authorship_citation_overview.Rdata")



##Race Cateogry----
RC=as.data.frame(table(article.data$AR_2))
RC=RC[-17,]

E1=ggplot(data=RC,aes(x=reorder(Var1,-Freq), y=Freq) )+
  geom_bar(stat="identity",fill="steelblue") +
  xlab("Ethnicity Categories")+
  ylab("Paper No.")+
  ggtitle("Tropical Medicine Authorship by Ethnic Groups")+
  geom_text(aes(label=Freq),vjust=-0.3,size=3.5)

ggsave("~/switchdrive/DataAnalysis /Github_R_Link_Test/Outcome/Plot/Tropical Medicine Authorship by Ethnicity Categories.jpg",
       width = 14,
       height = 5)

fa=data.frame(
  EthinicGroup=factor(c("Anglophone Regions","Europe","Asia","Africa")),
  Number=c(12665,23053,15018,2657),
  color=c("#994455","#997700","#EE99AA","#6699CC")
)

f1=ggplot(data=fa,aes(x=EthinicGroup, y=Number) )+
  geom_bar(stat="identity",fill=fa$color)+
  xlab("Ethinic Groups")+
  ylab("Author Numbers")+
  ggtitle("Ethnicities of First Authors")+
  geom_text(aes(label=Number),vjust=-0.3,size=3.5)

la=data.frame(
  EthinicGroup=factor(c("Anglophone Regions","Europe","Asia","Africa")),
  Number=c(15241,23329,13537,2162),
  color=c("#994455","#997700","#EE99AA","#6699CC")
)

l1=ggplot(data=la,aes(x=EthinicGroup, y=Number) )+
  geom_bar(stat="identity",fill=fa$color)+
  xlab("Ethinic Groups")+
  ylab("Author Numbers")+
  ggtitle("Ethnicities of Last Authors")+
  geom_text(aes(label=Number),vjust=-0.3,size=3.5)
f1/l1


-----
ggsave("~/switchdrive/DataAnalysis /Github_R_Link_Test/Outcome/Plot/Authorship by Ethnicity Categories.jpg",
       width = 10,
       height = 8)


RC.Cited=data.frame(EthnicityCategory=c("EU/EU",
                                        "EU/Asia",
                                        "EU/Anglophone",
                                        "EU/Africa",
                                        "Asia/EU",
                                        "Asia/Asia",
                                        "Asia/Anglophone",
                                        "Asia/Africa",
                                        "Anglophone/EU",
                                        "Anglophone/Asia",
                                        "Anglophone/Africa",
                                        "Anglophone/ Anglophone",
                                        "Africa/EU",
                                        "Africa/Asia",
                                        "Africa/Anglophone",
                                        "Africa/Africa") ,
                    PaperNo.=c(44382,
                               958,
                               5158,
                               191,
                               1475,
                               21569,
                               2692,
                               219,
                               3940,
                               1412,
                               852,
                               38879,
                               468,
                               227,
                               1025,
                               4895))

E2=ggplot(data=RC.Cited,aes(x=reorder(EthnicityCategory,-PaperNo.), y=PaperNo.) )+
  geom_bar(stat="identity",fill="steelblue") +
  xlab("Ethnicity Categories")+
  ylab("Cited Times.")+
  ggtitle("Tropical Medicine Cited by Ethnic Groups")+
  geom_text(aes(label=PaperNo.),vjust=-0.3,size=3.5)

E1/E2
ggsave("~/switchdrive/DataAnalysis /Github_R_Link_Test/Outcome/Plot/Ethnicity Group.jpg",
       width = 20,
       height = 10)


##SwissTPH----
fa1=data.frame(
  EthinicGroup=factor(c("Anglophone Regions","Europe","Asia","Africa")),
  Number=c(57,302,38,12),
  color=c("#994455","#997700","#EE99AA","#6699CC")
)
f2=ggplot(data=fa1,aes(x=EthinicGroup, y=Number) )+
  geom_bar(stat="identity",fill=fa$color)+
  xlab("Ethinic Groups")+
  ylab("Author Numbers")+
  ggtitle("Ethnicities of First Authors in Swiss TPH")+
  geom_text(aes(label=Number),vjust=-0.3,size=3.5)
la1=data.frame(
  EthinicGroup=factor(c("Anglophone Regions","Europe","Asia","Africa")),
  Number=c(76,422,18,5),
  color=c("#994455","#997700","#EE99AA","#6699CC")
)
l2=ggplot(data=la1,aes(x=EthinicGroup, y=Number) )+
  geom_bar(stat="identity",fill=fa$color)+
  xlab("Ethinic Groups")+
  ylab("Author Numbers")+
  ggtitle("Ethnicities of Last Authors in Swiss TPH ")+
  geom_text(aes(label=Number),vjust=-0.3,size=3.5)
f2/l2
ggsave("~/switchdrive/DataAnalysis /Github_R_Link_Test/Outcome/Plot/SwissTPH:Authorship by Ethnic Groups.jpg",
       width = 10,
       height = 8)

##Cited----
cfa1=data.frame(
  EthinicGroup=factor(c("Anglophone Regions","Europe","Asia","Africa")),
  Number=c(6007,7542,3887,856),
  color=c("#994455","#997700","#EE99AA","#6699CC")
)

cf2=ggplot(data=cfa1,aes(x=reorder(EthinicGroup,-Number), y=Number) )+
  geom_bar(stat="identity",fill=fa$color)+
  xlab("Ethinic Groups of First Authors")+
  ylab("Cited Times")+
  ggtitle("First Authors’ Ethnicities in TP Reference Lists")+
  geom_text(aes(label=Number),vjust=-0.3,size=3.5)


cla1=data.frame(
  EthinicGroup=factor(c("Anglophone Regions","Europe","Asia","Africa")),
  Number=c(6433,7441,3689,729),
  color=c("#994455","#997700","#EE99AA","#6699CC")
)
cl2=ggplot(data=cla1,aes(x=reorder(EthinicGroup,-Number), y=Number) )+
  geom_bar(stat="identity",fill=fa$color)+
  xlab("Ethinic Groups of Last Authors")+
  ylab("Cited Times")+
  ggtitle("Last Authors' Ethnicities in TP Reference Lists")+
  geom_text(aes(label=Number),vjust=-0.3,size=3.5)

##SwissTPH----

cfa.tph=data.frame(
  EthinicGroup=factor(c("Anglophone Regions","Europe","Asia","Africa")),
  Number=c(1032,
           172,
           75,
           26),
  color=c("#994455","#997700","#EE99AA","#6699CC")
)  

cf.tph=ggplot(data=cfa.tph,aes(x=reorder(EthinicGroup,-Number), y=Number) )+
  geom_bar(stat="identity",fill=fa$color)+
  xlab("Ethinic Groups of First Authors")+
  ylab("Cited Times")+
  ggtitle("First Authors' Ethnicities in Swiss TPH Reference Lists")+
  geom_text(aes(label=Number),vjust=-0.3,size=3.5)

cla.tph=data.frame(
  EthinicGroup=factor(c("Anglophone Regions","Europe","Asia","Africa")),
  Number=c(1018,
           236,
           84,
           23),
  color=c("#994455","#997700","#EE99AA","#6699CC")
)
cl.tph=ggplot(data=cla.tph,aes(x=reorder(EthinicGroup,-Number), y=Number) )+
  geom_bar(stat="identity",fill=fa$color)+
  xlab("Ethinic Groups of Last Authors")+
  ylab("Cited Times")+
  ggtitle("Last Authors' Ethnicities in Swiss TPH Reference Lists")+
  geom_text(aes(label=Number),vjust=-0.3,size=3.5)

ggarrange(cf2, cl2, cf.tph,cl.tph,
                  labels = c("A", "B", "C","D"),
                  ncol = 2, nrow = 2)+
  plot_annotation(title = "First/last Authors’ Ethnicities in Reference Lists",
                  theme = theme(plot.title = element_text(hjust = 0.5)))

ggsave("~/switchdrive/DataAnalysis /Github_R_Link_Test/Outcome/Plot/Citation by Ethnic Groups.jpg",
       width = 11,
       height = 8)


