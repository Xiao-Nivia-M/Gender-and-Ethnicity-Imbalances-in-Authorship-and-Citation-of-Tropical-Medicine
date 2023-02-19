# 1. Library Importation----
library(ggplot2)
library(ggsci)
library(tidyverse)
library(webr)
library(dplyr)
library(gridExtra)
library(ggpubr)
library(patchwork)

# 2. Overview of Tropical Medicine Publications from 1945 to 2021----
## 2.1 Data Importation from Step 3----
load("~/switchdrive/DataAnalysis /Github_R_Link_Test/df3_articledata.RData")

## 2.2 Publication Numbers by Year----
year.no=as.data.frame(table(article.data$PY))
year.no$Var1 <- as.numeric(as.character(year.no$Var1))
ggplot(data=year.no,aes(x=Var1, y=Freq))+
  geom_bar(stat = "identity", color="White", fill="steelblue")+
  scale_y_continuous(breaks = seq(from = 0, to = 6500, by = 500))+
  xlab("Year")+
  ylab("Publication No.")+
  ggtitle("Temporal Development of Tropical Medicine Publications")+
  theme(axis.text.x = element_text(angle=90, size=10,face="bold"),
        plot.title = element_text(face = "bold")) +
  scale_x_continuous(breaks = seq(1945, 2022, by = 10))+
  annotate("text",x=2017,y=5500, label = "5376")+
  annotate("text",x=1959, y=300, label="95")

ggsave("~/switchdrive/DataAnalysis /Github_R_Link_Test/Outcome/Plot/Temporal Development of Tropical Medicine Publications.jpg",
       width = 7,
       height = 5)

for (i in (1:nrow(year.no))){
  year.no$gap[i]=year.no$Freq[i+1]-year.no$Freq[i]
}






### 2.2.1 Fig2 ----

## 2.3 Publication Numbers by Journal (over 1000 papers)----
journal.rank=as.data.frame(table(article.data$SO))
journal.rank=subset(journal.rank,journal.rank$Freq>1000)

ggplot(data=journal.rank, aes(x=Freq, y=reorder(Var1,+Freq))) +
  geom_bar(stat = "identity", color="White", fill="steelblue")+
  xlab("Publication No.")+
  ylab("Journal Names")+
  theme(axis.text.y = element_text(size=10,face="bold"),
        axis.text.x = element_text(size=10,face="bold"),
        plot.title = element_text(face = "bold"))+
  geom_text(aes(label=Freq),vjust=-0.3,size=4)
ggsave("~/switchdrive/DataAnalysis /Github_R_Link_Test/Outcome/Plot/Top 22 Tropical Medicine by Number of Publications.jpg",
       width = 15,
       height = 8)
### 2.3.1 Fig2 ----

##2.4 Targeted Publications: Citing Paper and Cited Paper----

X=length(which(!is.na(article.data$DI))) # citing paper with DOI
Y=nrow(article.data) #citing paper no. without DOI 

#How many cited papers in tropical medicine - from step12 cited.articles 
## papers are the cited paper 
## papers are from tropical medicine 
z=length(unique(cited.article$Index))


### 2.4.1 Table 4 -----

# 3. Gender Assignment Result----
gender.all=as.data.frame(table(article.data$AG))
gender.all$Category=c("w/o unknown","w/ unknown","w/o unknown","w/ unknown","w/ unknown","w/ unknown",
                 "w/o unknown","w/ unknown","w/o unknown")
colnames(gender.all)=c("GenderAssignment","Freq","Category")
gender.all$Color= c("darkslateblue","darkslateblue","darkslategray4","lightcyan3",
                    "gray","darkslategray4","lightcyan3","lightsalmon3","lightsalmon3")
gender.assignment=gender.all %>%
  group_by(Category,GenderAssignment,Color)%>%
  summarise(n=sum(Freq))

###3.1 Bar chart of known and known ----
gender.unknown=sum(gender.assignment$n[gender.assignment$Category=="w/ unknown"])
gender.known=sum(gender.assignment$n[gender.assignment$Category=="w/o unknown"])

overall=gender.assignment %>%
  group_by(Category) %>%s
  summarise(Freq=sum(n))

Fig3.1=ggplot(data=overall, aes(x=Category,y=Freq))+
  geom_bar(stat = "identity",fill=c("gray","steelblue"))+
  xlab("Gender Category")+
  ylab("Frequency")+
  ggtitle("3.1 Gender Assignment Overview")+
  theme(axis.text.x = element_text(size=8,face="bold"),
        plot.title = element_text(face = "bold"))+
  geom_text(aes(label=Freq),vjust=-0.3,size=3)

###3.1.1 Fig3.1-----

###3.2 Bar chart of known----
g1=subset(gender.assignment,gender.assignment$Category=="w/o unknown")

Fig3.2=ggplot(data=g1, aes(x=reorder(GenderAssignment,-n),y=n))+
  geom_bar(stat = "identity",fill=Color)+
  xlab("Gender Category")+
  ylab("Frequency")+
  ggtitle("3.2 Gender Assignment w/o Unknown")+
  theme(axis.text.x = element_text(size=8,face="bold"),
        plot.title = element_text(face = "bold"))+
  geom_text(aes(label=n),vjust=-0.3,size=3)

###3.2.1 Fig3.2-----

###3.3 Bar chart of unknown----
g2=subset(gender.assignment,gender.assignment$Category=="w/ unknown")

Fig3.3=ggplot(data=g2, aes(x=reorder(GenderAssignment,-n),y=n))+
  geom_bar(stat = "identity",fill=g2$Color)+
  xlab("Gender Category")+
  ylab("Frequency")+
  ggtitle("3.3 Gender Assignment w/ Unknown")+
  theme(axis.text.x = element_text(size=8,face="bold"),
        plot.title = element_text(face = "bold"))+
  geom_text(aes(label=n),vjust=-0.3,size=3)
###3.3.1 Fig3.3-----

#4. Original Region Assignment Result-----
load("df7_articledata_withraces.RData")
###4.1 Bar chart of known and known ----






#Citations 
table(is.na(article.data$CP))
