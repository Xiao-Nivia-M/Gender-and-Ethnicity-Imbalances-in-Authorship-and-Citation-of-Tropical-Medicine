# 1. Library Importation----
library(ggplot2)
library(mgcv)
library(patchwork)
library(boot)
library(pbmcapply)
library(data.table)
library(quantreg)

# 2. Data Importation----
# Load in data from step 10
load("df9_articledata_propdata.RData")
load("Outcome/ResultingVariables/Step4.1_DataProcessing_TPH/df_1009_TPH_final.Rdata")

# 3. System Setting----
# Save number of cores on machine
cores=detectCores()

# 4.  Authorship: gender distribution  ----
## 4.1. Overall Tropical Medicine ----
article.gender.data=article.data[which(!is.na(article.data$GC)),]
df=as.data.frame(table(article.gender.data$AG))
colnames(df)=c("GenderCategories","Frequency")
Color=c("darkslateblue","darkslategray4",
              "lightcyan3","lightsalmon3")
p<-ggplot(data=df, aes(x=GenderCategories, y=Frequency)) +
  geom_bar(stat="identity",fill=Color)+
  theme_minimal()
# = Fig3.2


## 4.2. Tropical Medicine: Temporal Gender Categories ----
article.data=article.data[complete.cases(article.data$PY),]
timedata=NULL
for(i in unique(article.data$PY)){
  subgends=article.data$AG[article.data$PY==i & 
                             article.data$AG%in%c("MM","WM","MW","WW")]
  subgends=na.omit(subgends)
  timedata.i=data.frame(Year=rep(i,4),Gender=c("MM","WM","MW","WW"),
                        Prop=c(mean(subgends=="MM"),
                               mean(subgends=="WM"),
                               mean(subgends=="MW"),
                               mean(subgends=="WW")),
                        Color=c("darkslateblue","darkslategray4",
                                "lightcyan3","lightsalmon3"))
  timedata=rbind(timedata,timedata.i)}

timedata$Color=as.character(timedata$Color)
f1plot(timedata,"Temproal Trend of Gender Categories in TP Authorship")

ggsave("~/switchdrive/DataAnalysis /Github_R_Link_Test/Outcome/Plot/Temproal Trend of Gender Categories in TP Authorship.jpg",
       width = 7,
       height = 5)

w.data=timedata[timedata$Gender%in%c("WM","MW","WW"),]
write_xlsx(w.data,"w.xlsx")

w=read.csv("w.csv",stringsAsFactors=F)
t.test(w$rate, alternative="two.sided",conf.level=0.95)


##4.3 1st author's gender----
article.data$fa_gender=substr(article.data$AG,1,1)

##4.4 last author's gender----
article.data$la_gender=substr(article.data$AG,2,2)

#5. Citation Analysis----

##5.1 Data Preparation-----
## Find subset of articles for analysis
# 1) observed gender proportions (cols 1-4),
# 2) expected proportions under unconditional null (cols 5-8),
# 3) expected proportions under conditional null (cols 9-12), and
# 4) total number of non-self-authored candidate citations (col 13)

label.MM.observed=c(Type="Observed:MM")
label.WM.observed=c(Type="Observed:WM")
label.MW.observed=c(Type="Observed:MW")
label.WW.observed=c(Type="Observed:WW")

label.MM.uncond=c(Type="uncond:MM")
label.WM.uncond=c(Type="uncond:WM")
label.MW.uncond=c(Type="uncond:MW")
label.WW.uncond=c(Type="uncond:WW")

label.MM.cond=c(Type="cond:MM")
label.WM.cond=c(Type="cond:WM")
label.MW.cond=c(Type="cond:MW")
label.WW.cond=c(Type="cond:WW")

label.citedno=c(Type="nonselfcite")

##time range: 1945-2021
time_window=article.data$PY%in%c(1945:2021)
has_citations=ref_proportions[,13]>0
subset_articles=time_window & has_citations
# Get subset of ref_proportions data
ref_prop_sub=ref_proportions[subset_articles,]
ref_tot_sub=ref_prop_sub[,1:12]*ref_prop_sub[,13]

# Gap relative to overall literature
citeprops(ref_tot_sub,type='randomdraw')
citegap(ref_tot_sub,type='randomdraw')


# Gap conditional on papers' characteristics
citeprops(ref_tot_sub,type='conditional')
citegap(ref_tot_sub,type='conditional')

## 5.2 Overall-----

# Get bootstrap standard errors for gap values
boot.rd=boot(ref_tot_sub,citegap,R=1000,type='randomdraw')
boot.cn=boot(ref_tot_sub,citegap,R=1000,type='conditional')

# Create ggplot compatible data frames
plot.df.randomdraw=get.plotdf(boot.rd)
p.all.rd=f2plot(plot.df.randomdraw,"Citation Gap Relative to Literature",
                ymin=-0.35,ymax=0.35)
ggsave("~/switchdrive/DataAnalysis /Github_R_Link_Test/Outcome/Plot/Citation Gap Relative to Literature.jpg",
       width = 7,
       height = 5)


plot.df.conditional=get.plotdf(boot.cn)
p.all.cn=f2plot(plot.df.conditional,"Citation Gap Conditional on Characteristics",
                ymin=-0.15,ymax=0.1)

ggsave("~/switchdrive/DataAnalysis /Github_R_Link_Test/Outcome/Plot/Citation Gap Conditonal in Characteristics.jpg",
       width = 7,
       height = 5)

# View plots
p.all.rd
p.all.cn


##5.3 Temporal change in citation gap-----
### 1. 1945-1955: Over/under citation ----
time_window=article.data$PY%in%c(1945:1955)
has_citations=ref_proportions[,13]>0
subset_articles=time_window & has_citations

# Get subset of ref_proportions data
ref_prop_sub=ref_proportions[subset_articles,]
ref_tot_sub=ref_prop_sub[,1:12]*ref_prop_sub[,13]

#### 1.1 Random-----
# Gap relative to overall literature
citeprops(ref_tot_sub,type='randomdraw')
citegap(ref_tot_sub,type='randomdraw')

# Get bootstrap standard errors for gap values
boot.rd=boot(ref_tot_sub,citegap,R=1000,type='randomdraw')

# Create ggplot compatible data frames
plot.df.randomdraw=get.plotdf(boot.rd)

#### 1.2 Condition----
# Gap conditional on papers' characteristics
citeprops(ref_tot_sub,type='conditional')
citegap(ref_tot_sub,type='conditional')

# Get bootstrap standard errors for gap values
boot.cn=boot(ref_tot_sub,citegap,R=1000,type='conditional')

# Create ggplot compatible data frames
plot.df.conditional=get.plotdf(boot.cn)


### 2. 1955-1965: Over/under citation ----
time_window=article.data$PY%in%c(1955:1965)
has_citations=ref_proportions[,13]>0
subset_articles=time_window & has_citations

# Get subset of ref_proportions data
ref_prop_sub=ref_proportions[subset_articles,]
ref_tot_sub=ref_prop_sub[,1:12]*ref_prop_sub[,13]

#### 2.1 Random-----
# Gap relative to overall literature
citeprops(ref_tot_sub,type='randomdraw')
citegap(ref_tot_sub,type='randomdraw')

# Get bootstrap standard errors for gap values
boot.rd=boot(ref_tot_sub,citegap,R=1000,type='randomdraw')

# Create ggplot compatible data frames
plot.df.randomdraw=get.plotdf(boot.rd)


#### 2.2 Condition----
# Gap conditional on papers' characteristics
citeprops(ref_tot_sub,type='conditional')
citegap(ref_tot_sub,type='conditional')

# Get bootstrap standard errors for gap values
boot.cn=boot(ref_tot_sub,citegap,R=1000,type='conditional')

# Create ggplot compatible data frames
plot.df.conditional=get.plotdf(boot.cn)




### 3. 1965-1975: Over/under citation ----
time_window=article.data$PY%in%c(1965:1975)
has_citations=ref_proportions[,13]>0
subset_articles=time_window & has_citations

# Get subset of ref_proportions data
ref_prop_sub=ref_proportions[subset_articles,]
ref_tot_sub=ref_prop_sub[,1:12]*ref_prop_sub[,13]

#### 3.1 Random----
# Gap relative to overall literature
citeprops(ref_tot_sub,type='randomdraw')
citegap(ref_tot_sub,type='randomdraw')

# Get bootstrap standard errors for gap values
boot.rd=boot(ref_tot_sub,citegap,R=1000,type='randomdraw')

# Create ggplot compatible data frames
plot.df.randomdraw=get.plotdf(boot.rd)


#### 3.2 Condition----
# Gap conditional on papers' characteristics
citeprops(ref_tot_sub,type='conditional')
citegap(ref_tot_sub,type='conditional')

# Get bootstrap standard errors for gap values
boot.cn=boot(ref_tot_sub,citegap,R=1000,type='conditional')

# Create ggplot compatible data frames
plot.df.conditional=get.plotdf(boot.cn)
plot.df.conditional


### 4. 1975-1985: Over/under citation ----
time_window=article.data$PY%in%c(1975:1985)
has_citations=ref_proportions[,13]>0
subset_articles=time_window & has_citations

# Get subset of ref_proportions data
ref_prop_sub=ref_proportions[subset_articles,]
ref_tot_sub=ref_prop_sub[,1:12]*ref_prop_sub[,13]

#### 4.1 Random----
# Gap relative to overall literature
citeprops(ref_tot_sub,type='randomdraw')
citegap(ref_tot_sub,type='randomdraw')

# Get bootstrap standard errors for gap values
boot.rd=boot(ref_tot_sub,citegap,R=1000,type='randomdraw')

# Create ggplot compatible data frames
plot.df.randomdraw=get.plotdf(boot.rd)

#### 4.2 Condition----
# Gap conditional on papers' characteristics
citeprops(ref_tot_sub,type='conditional')
citegap(ref_tot_sub,type='conditional')

# Get bootstrap standard errors for gap values
boot.cn=boot(ref_tot_sub,citegap,R=1000,type='conditional')

# Create ggplot compatible data frames
plot.df.conditional=get.plotdf(boot.cn)
plot.df.conditional



### 5. 1985-1995: Over/under citation ----
time_window=article.data$PY%in%c(1985:1995)
has_citations=ref_proportions[,13]>0
subset_articles=time_window & has_citations


# Get subset of ref_proportions data
ref_prop_sub=ref_proportions[subset_articles,]
ref_tot_sub=ref_prop_sub[,1:12]*ref_prop_sub[,13]

#### 5.1 Random----
# Gap relative to overall literature
citeprops(ref_tot_sub,type='randomdraw')
citegap(ref_tot_sub,type='randomdraw')

# Get bootstrap standard errors for gap values
boot.rd=boot(ref_tot_sub,citegap,R=1000,type='randomdraw')

# Create ggplot compatible data frames
plot.df.randomdraw=get.plotdf(boot.rd)



#### 5.2 Condition----
# Gap conditional on papers' characteristics
citeprops(ref_tot_sub,type='conditional')
citegap(ref_tot_sub,type='conditional')

# Get bootstrap standard errors for gap values
boot.cn=boot(ref_tot_sub,citegap,R=1000,type='conditional')

# Create ggplot compatible data frames
plot.df.conditional=get.plotdf(boot.cn)
plot.df.conditional



### 6. 1995-2005: Over/under citation ----
time_window=article.data$PY%in%c(1995:2005)
has_citations=ref_proportions[,13]>0
subset_articles=time_window & has_citations

# Get subset of ref_proportions data
ref_prop_sub=ref_proportions[subset_articles,]
ref_tot_sub=ref_prop_sub[,1:12]*ref_prop_sub[,13]

#### 6.1 Random----
# Gap relative to overall literature
citeprops(ref_tot_sub,type='randomdraw')
citegap(ref_tot_sub,type='randomdraw')

# Get bootstrap standard errors for gap values
boot.rd=boot(ref_tot_sub,citegap,R=1000,type='randomdraw')

# Create ggplot compatible data frames
plot.df.randomdraw=get.plotdf(boot.rd)

#### 6.2 Condition----
# Gap conditional on papers' characteristics
citeprops(ref_tot_sub,type='conditional')
citegap(ref_tot_sub,type='conditional')

# Get bootstrap standard errors for gap values
boot.cn=boot(ref_tot_sub,citegap,R=1000,type='conditional')

# Create ggplot compatible data frames
plot.df.conditional=get.plotdf(boot.cn)
plot.df.conditional



### 7. 2005-2015: Over/under citation ----
time_window=article.data$PY%in%c(2005:2015)
has_citations=ref_proportions[,13]>0
subset_articles=time_window & has_citations

# Get subset of ref_proportions data
ref_prop_sub=ref_proportions[subset_articles,]
ref_tot_sub=ref_prop_sub[,1:12]*ref_prop_sub[,13]

#### 7.1 Random----

# Gap relative to overall literature
citeprops(ref_tot_sub,type='randomdraw')
citegap(ref_tot_sub,type='randomdraw')

# Get bootstrap standard errors for gap values
boot.rd=boot(ref_tot_sub,citegap,R=1000,type='randomdraw')

# Create ggplot compatible data frames
plot.df.randomdraw=get.plotdf(boot.rd)

#### 7.2 Condition----
# Gap conditional on papers' characteristics
citeprops(ref_tot_sub,type='conditional')
citegap(ref_tot_sub,type='conditional')

# Get bootstrap standard errors for gap values
boot.cn=boot(ref_tot_sub,citegap,R=1000,type='conditional')

# Create ggplot compatible data frames
plot.df.conditional=get.plotdf(boot.cn)
plot.df.conditional




### 8. 2015-2021: Over/under citation ----
time_window=article.data$PY%in%c(2015:2021)
has_citations=ref_proportions[,13]>0
subset_articles=time_window & has_citations

# Get subset of ref_proportions data
ref_prop_sub=ref_proportions[subset_articles,]
ref_tot_sub=ref_prop_sub[,1:12]*ref_prop_sub[,13]

#### 8.1 Random----
# Gap relative to overall literature
citeprops(ref_tot_sub,type='randomdraw')
citegap(ref_tot_sub,type='randomdraw')

# Get bootstrap standard errors for gap values
boot.rd=boot(ref_tot_sub,citegap,R=1000,type='randomdraw')

# Create ggplot compatible data frames
plot.df.randomdraw=get.plotdf(boot.rd)


#### 8.2 Condition----
# Gap conditional on papers' characteristics
citeprops(ref_tot_sub,type='conditional')
citegap(ref_tot_sub,type='conditional')

# Get bootstrap standard errors for gap values
boot.cn=boot(ref_tot_sub,citegap,R=1000,type='conditional')

# Create ggplot compatible data frames
plot.df.conditional=get.plotdf(boot.cn)
plot.df.conditional


##6. TPH Citation Analysis ----
### 6.1 Data Preparation -----
## Time range: 1945-2021
time_window=article.data$PY%in%c(1945:2021)
tph=article.data$TPH
has_citations=ref_proportions[,13]>0
subset_articles=time_window & has_citations&tph

# Get subset of ref_proportions data
ref_prop_sub=ref_proportions[subset_articles,]
ref_tot_sub=ref_prop_sub[,1:12]*ref_prop_sub[,13]

 # Gap relative to overall literature
citeprops(ref_tot_sub,type='randomdraw')
citegap(ref_tot_sub,type='randomdraw')

# Gap conditional on papers' characteristics
citeprops(ref_tot_sub,type='conditional')
citegap(ref_tot_sub,type='conditional')

# Get bootstrap standard errors for gap values
boot.rd=boot(ref_tot_sub,citegap,R=500,type='randomdraw')
boot.cn=boot(ref_tot_sub,citegap,R=500,type='conditional')

# Create ggplot compatible data frames
plot.df.randomdraw=get.plotdf(boot.rd)

p.all.rd.tph=f2plot(plot.df.randomdraw,"Swiss TPH Gap relative to Literature",
                ymin=-0.50,ymax=0.50)

ggsave("~/switchdrive/DataAnalysis /Github_R_Link_Test/Outcome/Plot/Swiss TPH Gap relative to Literature.jpg",
       width = 7,
       height = 5)


plot.df.conditional=get.plotdf(boot.cn)
p.all.cn.tph=f2plot(plot.df.conditional,"Swiss TPH Gap Conditional on Characteristics",
                ymin=-0.5,ymax=0.50)

# View plots
p.all.rd.tph/ p.all.cn.tph

ggsave("~/switchdrive/DataAnalysis /Github_R_Link_Test/Outcome/Plot/Swiss TPH Gap.jpg",
       width = 7,
       height = 5)



