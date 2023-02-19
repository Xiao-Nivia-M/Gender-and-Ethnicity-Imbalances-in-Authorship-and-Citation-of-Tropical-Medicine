#1. Library Importation ----
library(pbmcapply)
library(ggpubr)

#2. Data Importation and System Setting----
options(scipen=999)
load("df8_articledata_expecdata.RData")
load("Outcome/ResultingVariables/Step4.1_DataProcessing_TPH/df_1009_TPH_final.Rdata")

#3. Observed Citation Behavior: Gender----
##3.1 Whole Tropical Medicine----
###3.1.1: without self-citation -----
gender.df=data.frame(Year=NA,MM=NA,WM=NA,MW=NA,WW=NA)
i=0
for (x in 1:nrow(article.data)){
  cited.papers=strsplit(article.data$CP[x],", ")[[1]]
  self.authored=strsplit(article.data$SA[x],", ")[[1]]
  cited.notself=as.numeric(cited.papers[!(cited.papers%in%self.authored)])
  cited.notself=cited.notself[!is.na(article.data$GC[cited.notself])]
  i=i+1
  if(length(cited.notself)>0){
    cited.genders=article.data$GC[cited.notself]
    gender.table=table(factor(cited.genders,lev=0:3))
    gender.df[i,1]=article.data$PY[x]
    gender.df[i,2:5]=as.vector(gender.table)
  }else{
    gender.table=table(factor(NA,lev=0:3))
    gender.df[i,1]=article.data$PY[x]
    gender.df[i,2:5]=as.vector(gender.table)
  }
}
summary(gender.df)

sum.df=data.frame(GenderCategories=NA,Frequency=NA)
sum.df[1,]=c("MM",sum(gender.df$MM))
sum.df[2,]=c("WM",sum(gender.df$WM))
sum.df[3,]=c("MW",sum(gender.df$MW))
sum.df[4,]=c("WW",sum(gender.df$WW))

sum.df$Frequency=as.numeric(sum.df$Frequency)

tp.cited=ggplot(data=sum.df, aes(x=reorder(GenderCategories,-Frequency), y=Frequency,label=Frequency)) +
  geom_bar(stat="identity",fill=Color)+
  theme_minimal()+
  #geom_text(size = 3, position = position_stack(vjust = 0.5))+
  ggtitle("Overall Tropical Medicine Gender Categories in Citaion (without self-citation)")+
  theme(axis.text.x = element_text(size=8,face="bold"),
        plot.title = element_text(face = "bold"))+
  geom_text(aes(label=Frequency),vjust=-0.3,size=3)+
  xlab("Gender Category")+
  ylab("Frequency")

ggsave("~/switchdrive/DataAnalysis /Github_R_Link_Test/Outcome/Plot/Overall Tropical Medicine Gender Categories in Citaion (without self-citation).jpg",
       width = 7,
       height = 5)

tp.cited #section 3.3.1 (Fig 8)

###3.1.2: with self-citation----
self.gender.df=data.frame(Year=NA,MM=NA,WM=NA,MW=NA,WW=NA)
i=0
for (x in 1:nrow(article.data)){
  cited.papers=strsplit(article.data$CP[x],", ")[[1]]
  cited.w.self=as.numeric(cited.papers)
  cited.w.self=cited.w.self[!is.na(article.data$GC[cited.w.self])]
  i=i+1
  if(length(cited.w.self)>0){
    cited.genders=article.data$GC[cited.w.self]
    gender.table=table(factor(cited.genders,lev=0:3))
    self.gender.df[i,1]=article.data$PY[x]
    self.gender.df[i,2:5]=as.vector(gender.table)
  }else{
    gender.table=table(factor(NA,lev=0:3))
    self.gender.df[i,1]=article.data$PY[x]
    self.gender.df[i,2:5]=as.vector(gender.table)
  }
}
summary(self.gender.df)

self.sum.df=data.frame(GenderCategories=NA,Frequency=NA)
self.sum.df[1,]=c("MM",sum(self.gender.df$MM))
self.sum.df[2,]=c("WM",sum(self.gender.df$WM))
self.sum.df[3,]=c("MW",sum(self.gender.df$MW))
self.sum.df[4,]=c("WW",sum(self.gender.df$WW))

self.sum.df$Frequency=as.numeric(self.sum.df$Frequency)

self.tp.cited=ggplot(data=self.sum.df, aes(x=reorder(GenderCategories,-Frequency), y=Frequency,label=Frequency)) +
  geom_bar(stat="identity",fill=Color)+
  theme_minimal()+
  #geom_text(size = 3, position = position_stack(vjust = 0.5))+
  ggtitle("Overall Tropical Medicine Gender Categories in Citaion (with self-citation)")+
  xlab("Gender Category")+
  ylab("Frequency")+
  geom_text(aes(label=Frequency),vjust=-0.3,size=3)+
  theme(axis.text.x = element_text(size=8,face="bold"),
        plot.title = element_text(face = "bold"))

self.tp.cited #section 3.3.1 (Fig 9)

###3.1.3: Result ----
#section 3.3.1
tp.cited #[Fig8]
self.tp.cited #[Fig 9]


#4. Temporal Cited Gender Category -----
##4.1 Whole Tropical Medicine w/o self-citation----
View(gender.df)
gender.df=na.omit(gender.df)
cited.timedata=NULL
for(i in unique(gender.df$Year)){
  sub=gender.df[gender.df$Year==i,]
  all=sum(sub$MM)+sum(sub$WM)+sum(sub$MW)+sum(sub$WW)
  cited.timedata.i=data.frame(Year=rep(i,4),Gender=c("MM","WM","MW","WW"),
                              Prop=c(sum(sub$MM)/all,
                                    sum(sub$WM)/all,
                                    sum(sub$MW)/all,
                                    sum(sub$WW)/all),
                              Color=c("darkslateblue","darkslategray4",
                                      "lightcyan3","lightsalmon3"))
  cited.timedata=rbind(cited.timedata,cited.timedata.i)
}
cited.timedata$Color=as.character(cited.timedata$Color)
cited.timedata=na.omit(cited.timedata)
f1plot(cited.timedata,"Temporal Trend of Gender Categories in TP Citation without Self-Citation")

ggsave("~/switchdrive/DataAnalysis /Github_R_Link_Test/Outcome/Plot/Temporal Trend of Gender Categories in TP Citation without Self-Citation.jpg",
       width = 7,
       height = 5)
citedw.data=cited.timedata[cited.timedata$Gender%in%c("WM","MW","WW"),]
write_xlsx(citedw.data,"citedw.xlsx")
cw=read.csv("citedw.csv",stringsAsFactors=F)
t.test(cw$Sum.of.Prop, alternative="two.sided",conf.level=0.95)

###4.1.1 Result: Fig 10----

##4.2 Whole Tropical Medicine w/ self-citation (not shown)----
View(self.gender.df)
self.gender.df=na.omit(self.gender.df)

self.cited.timedata=NULL
for(i in unique(self.gender.df$Year)){
  sub=self.gender.df[self.gender.df$Year==i,]
  all=sum(sub$MM)+sum(sub$WM)+sum(sub$MW)+sum(sub$WW)
  self.cited.timedata.i=data.frame(Year=rep(i,4),Gender=c("MM","WM","MW","WW"),
                                   Prop=c(sum(sub$MM)/all,
                                          sum(sub$WM)/all,
                                          sum(sub$MW)/all,
                                          sum(sub$WW)/all),
                                   Color=c("darkslateblue","darkslategray4",
                                           "lightcyan3","lightsalmon3"))
  self.cited.timedata=rbind(self.cited.timedata,self.cited.timedata.i)
}
self.cited.timedata$Color=as.character(self.cited.timedata$Color)
self.cited.timedata=na.omit(self.cited.timedata)

f1plot(self.cited.timedata,"Tropical Medicine Citation by Year w/ Self-citation")

