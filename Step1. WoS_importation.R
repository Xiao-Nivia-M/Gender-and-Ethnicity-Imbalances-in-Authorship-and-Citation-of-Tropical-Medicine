# 1. Library Importation----
library(bibliometrix)
library(rvest)
library(dplyr)
library(xml2)
library(XML)



#import the data set 
folders=c("Src/TropicalMedicine_latest/1945")
folders=c("Src/TropicalMedicine_latest/1946")
folders=c("Src/TropicalMedicine_latest/1947")
folders=c("Src/TropicalMedicine_latest/1948")
folders=c("Src/TropicalMedicine_latest/1949")
folders=c("Src/TropicalMedicine_latest/1950")
folders=c("Src/TropicalMedicine_latest/1951")
folders=c("Src/TropicalMedicine_latest/1952")
folders=c("Src/TropicalMedicine_latest/1953")
folders=c("Src/TropicalMedicine_latest/1954")
folders=c("Src/TropicalMedicine_latest/1955")
folders=c("Src/TropicalMedicine_latest/1956")
folders=c("Src/TropicalMedicine_latest/1957")
folders=c("Src/TropicalMedicine_latest/1958")
folders=c("Src/TropicalMedicine_latest/1959")
folders=c("Src/TropicalMedicine_latest/1960")
folders=c("Src/TropicalMedicine_latest/1961")
folders=c("Src/TropicalMedicine_latest/1962")
folders=c("Src/TropicalMedicine_latest/1963")
folders=c("Src/TropicalMedicine_latest/1964")
folders=c("Src/TropicalMedicine_latest/1965")
folders=c("Src/TropicalMedicine_latest/1966")
folders=c("Src/TropicalMedicine_latest/1967")
folders=c("Src/TropicalMedicine_latest/1968")
folders=c("Src/TropicalMedicine_latest/1969")
folders=c("Src/TropicalMedicine_latest/1970")
folders=c("Src/TropicalMedicine_latest/1971")
folders=c("Src/TropicalMedicine_latest/1972")
folders=c("Src/TropicalMedicine_latest/1973")
folders=c("Src/TropicalMedicine_latest/1974")
folders=c("Src/TropicalMedicine_latest/1975")
folders=c("Src/TropicalMedicine_latest/1976")
folders=c("Src/TropicalMedicine_latest/1977")
folders=c("Src/TropicalMedicine_latest/1978")
folders=c("Src/TropicalMedicine_latest/1979")
folders=c("Src/TropicalMedicine_latest/1980")
folders=c("Src/TropicalMedicine_latest/1981")
folders=c("Src/TropicalMedicine_latest/1982")
folders=c("Src/TropicalMedicine_latest/1983")
folders=c("Src/TropicalMedicine_latest/1984")
folders=c("Src/TropicalMedicine_latest/1985")
folders=c("Src/TropicalMedicine_latest/1986")
folders=c("Src/TropicalMedicine_latest/1987")
folders=c("Src/TropicalMedicine_latest/1988")
folders=c("Src/TropicalMedicine_latest/1989")
folders=c("Src/TropicalMedicine_latest/1990")
folders=c("Src/TropicalMedicine_latest/1991")
folders=c("Src/TropicalMedicine_latest/1992")
folders=c("Src/TropicalMedicine_latest/1993")
folders=c("Src/TropicalMedicine_latest/1994")
folders=c("Src/TropicalMedicine_latest/1995")
folders=c("Src/TropicalMedicine_latest/1996")
folders=c("Src/TropicalMedicine_latest/1997")
folders=c("Src/TropicalMedicine_latest/1998")
folders=c("Src/TropicalMedicine_latest/1999")
folders=c("Src/TropicalMedicine_latest/2000")
folders=c("Src/TropicalMedicine_latest/2001")
folders=c("Src/TropicalMedicine_latest/2002")
folders=c("Src/TropicalMedicine_latest/2003")
folders=c("Src/TropicalMedicine_latest/2004")
folders=c("Src/TropicalMedicine_latest/2005")
folders=c("Src/TropicalMedicine_latest/2006")
folders=c("Src/TropicalMedicine_latest/2007")
folders=c("Src/TropicalMedicine_latest/2008")
folders=c("Src/TropicalMedicine_latest/2009")
folders=c("Src/TropicalMedicine_latest/2010")
folders=c("Src/TropicalMedicine_latest/2011")
folders=c("Src/TropicalMedicine_latest/2012")
folders=c("Src/TropicalMedicine_latest/2013")
folders=c("Src/TropicalMedicine_latest/2014")
folders=c("Src/TropicalMedicine_latest/2015")
folders=c("Src/TropicalMedicine_latest/2016")
folders=c("Src/TropicalMedicine_latest/2017")
folders=c("Src/TropicalMedicine_latest/2018")
folders=c("Src/TropicalMedicine_latest/2019")
folders=c("Src/TropicalMedicine_latest/2020")
folders=c("Src/TropicalMedicine_latest/2021")
for(i in folders){
  files=list.files(i)
  data.frame=NULL
  for(j in files){
    this.data.frame=readFiles(paste0(i,"/",j))
    this.data.frame=createdf(this.data.frame)
    if(!is.null(data.frame)){
      data.frame=merge(data.frame,this.data.frame,all=T,sort=F)
    }else{
      data.frame=this.data.frame
    }
  }
  
  save(data.frame,file=paste0(i,"_df1_webofscience.RData"))
}

# 3. Find missing DOI ----
i="Src/TropicalMedicine_latest/1950"#done
i="Src/TropicalMedicine_latest/1951"#done
i="Src/TropicalMedicine_latest/1952"#done
i="Src/TropicalMedicine_latest/1953"#done
i="Src/TropicalMedicine_latest/1954"#done
i="Src/TropicalMedicine_latest/1955"#done
i="Src/TropicalMedicine_latest/1956"#done
i="Src/TropicalMedicine_latest/1957"#done
i="Src/TropicalMedicine_latest/1958"#done
i="Src/TropicalMedicine_latest/1959"#done
i="Src/TropicalMedicine_latest/1960"#done
i="Src/TropicalMedicine_latest/1961"#done
i="Src/TropicalMedicine_latest/1962"#done
i="Src/TropicalMedicine_latest/1963"#done
i="Src/TropicalMedicine_latest/1964"#done
i="Src/TropicalMedicine_latest/1965"#done
i="Src/TropicalMedicine_latest/1966"#done
i="Src/TropicalMedicine_latest/1967"#done
i="Src/TropicalMedicine_latest/1968"#done
i="Src/TropicalMedicine_latest/1969"#done
i="Src/TropicalMedicine_latest/1970"#done
i="Src/TropicalMedicine_latest/1971"#done
i="Src/TropicalMedicine_latest/1972"#done
i="Src/TropicalMedicine_latest/1973"#done
i="Src/TropicalMedicine_latest/1974"#done
i="Src/TropicalMedicine_latest/1975"#done
i="Src/TropicalMedicine_latest/1976"#done
i="Src/TropicalMedicine_latest/1977"#done
i="Src/TropicalMedicine_latest/1978"#done
i="Src/TropicalMedicine_latest/1979"#done
i="Src/TropicalMedicine_latest/1980"#done
i="Src/TropicalMedicine_latest/1981"#done
i="Src/TropicalMedicine_latest/1982"#done
i="Src/TropicalMedicine_latest/1983"#done
i="Src/TropicalMedicine_latest/1984"#done
i="Src/TropicalMedicine_latest/1985"#done
i="Src/TropicalMedicine_latest/1986"#done
i="Src/TropicalMedicine_latest/1987"#done
i="Src/TropicalMedicine_latest/1988"#done
i="Src/TropicalMedicine_latest/1989"#done
i="Src/TropicalMedicine_latest/1990"#done
i="Src/TropicalMedicine_latest/1991"#done
i="Src/TropicalMedicine_latest/1992"#done
i="Src/TropicalMedicine_latest/1993"#done
i="Src/TropicalMedicine_latest/1994"#done
i="Src/TropicalMedicine_latest/1995"#done
i="Src/TropicalMedicine_latest/1996"#done
i="Src/TropicalMedicine_latest/1997"#done
i="Src/TropicalMedicine_latest/1998"#done
i="Src/TropicalMedicine_latest/1999"#done
i="Src/TropicalMedicine_latest/2000"#done
i="Src/TropicalMedicine_latest/2001"#done
i="Src/TropicalMedicine_latest/2002"#done
i="Src/TropicalMedicine_latest/2003"#done
i="Src/TropicalMedicine_latest/2004"#done
i="Src/TropicalMedicine_latest/2005"#done
i="Src/TropicalMedicine_latest/2006"#done
i="Src/TropicalMedicine_latest/2007"#done
i="Src/TropicalMedicine_latest/2008"#done
i="Src/TropicalMedicine_latest/2009"#done
i="Src/TropicalMedicine_latest/2010"#done
i="Src/TropicalMedicine_latest/2011"#done
i="Src/TropicalMedicine_latest/2012"#done
i="Src/TropicalMedicine_latest/2013"#done
i="Src/TropicalMedicine_latest/2014"#done
i="Src/TropicalMedicine_latest/2015"#done
i="Src/TropicalMedicine_latest/2016"#done
i="Src/TropicalMedicine_latest/2017"#done
i="Src/TropicalMedicine_latest/2018"#done
i="Src/TropicalMedicine_latest/2019"#done
i="Src/TropicalMedicine_latest/2020"#done
i="Src/TropicalMedicine_latest/2021"#done


load(paste0(i,"_df1_webofscience.RData"))
nrow(data.frame)
without.DOI=which((data.frame$DI=="" | is.na(data.frame$DI)) & 
                    !is.na(data.frame$PM))

if(length(without.DOI)>0){
  for(j in without.DOI){
    this.pubmed=data.frame$PM[j]
    turl=paste0("https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/?ids=",
                this.pubmed)
    xml=suppressWarnings(read_xml(turl,as_html=T))
    doi=xml %>% html_nodes("record") %>% html_attr("doi")
    if(is.na(doi)){
      turl=paste0("https://pubmed.ncbi.nlm.nih.gov/",this.pubmed)
      html=read_html(turl)
      doi=html %>% html_nodes("meta[name='citation_doi']") %>%
        html_attr("content")
    }
    doi=ifelse(!is.na(doi),doi,"")
    print(paste(j,"NA"))
    
    if(nchar(doi)>0){
      data.frame$DI[j]=doi
      print(doi)
    }
    Sys.sleep(2)
  }
}
without.DOI=which((data.frame$DI=="" | is.na(data.frame$DI)) & 
                    !is.na(data.frame$PM))
!is.na(data.frame$PD)
!is.na(data.frame$C1)
!is.na(data.frame$RP)

#when PD, CI, RP are missing, manually add the value 
#data.frame$PD=6
#data.frame$C1=0
#data.frame$RP=0

data.frame=data.frame %>% 
  select(AF, SO, DT, CR, TC, PD, PY, DI,C1,RP,TI)

#double check again 
#data.frame=data.frame %>% 
  #select(AF, SO, DT, CR, TC, PD, PY, DI,C1,RP,TI)

#change the month into numerics 
data.frame$PD=unlist(lapply(1:nrow(data.frame),get.date,pd=data.frame$PD))
data.frame$PD=as.numeric(data.frame$PD)
#data.frame=data.frame[data.frame$DT%in%c("Article","Review"),]
data.frame$DI=tolower(data.frame$DI)
data.frame$CR=tolower(data.frame$CR)
i
length(without.DOI)
save(data.frame,file=paste0(i,"_df1_webofscience.RData"))

# 4. Check how many publications without DOI after searching  ----
folders=c("Src/TropicalMedicine_latest/1945",
          "Src/TropicalMedicine_latest/1946",
          "Src/TropicalMedicine_latest/1947",
          "Src/TropicalMedicine_latest/1948",
          "Src/TropicalMedicine_latest/1949",
          "Src/TropicalMedicine_latest/1950",
          "Src/TropicalMedicine_latest/1951",
          "Src/TropicalMedicine_latest/1952",
          "Src/TropicalMedicine_latest/1953",
          "Src/TropicalMedicine_latest/1954",
          "Src/TropicalMedicine_latest/1955",
          "Src/TropicalMedicine_latest/1956",
          "Src/TropicalMedicine_latest/1957",
          "Src/TropicalMedicine_latest/1958",
          "Src/TropicalMedicine_latest/1959",
          "Src/TropicalMedicine_latest/1960",
          "Src/TropicalMedicine_latest/1961",
          "Src/TropicalMedicine_latest/1962",
          "Src/TropicalMedicine_latest/1963",
          "Src/TropicalMedicine_latest/1964",
          "Src/TropicalMedicine_latest/1965",
          "Src/TropicalMedicine_latest/1966",
          "Src/TropicalMedicine_latest/1967",
          "Src/TropicalMedicine_latest/1968",
          "Src/TropicalMedicine_latest/1969",
          "Src/TropicalMedicine_latest/1970",
          "Src/TropicalMedicine_latest/1971",
          "Src/TropicalMedicine_latest/1972",
          "Src/TropicalMedicine_latest/1973",
          "Src/TropicalMedicine_latest/1974",
          "Src/TropicalMedicine_latest/1975",
          "Src/TropicalMedicine_latest/1976",
          "Src/TropicalMedicine_latest/1977",
          "Src/TropicalMedicine_latest/1978",
          "Src/TropicalMedicine_latest/1979",
          "Src/TropicalMedicine_latest/1980",
          "Src/TropicalMedicine_latest/1981",
          "Src/TropicalMedicine_latest/1982",
          "Src/TropicalMedicine_latest/1983",
          "Src/TropicalMedicine_latest/1984",
          "Src/TropicalMedicine_latest/1985",
          "Src/TropicalMedicine_latest/1986",
          "Src/TropicalMedicine_latest/1987",
          "Src/TropicalMedicine_latest/1988",
          "Src/TropicalMedicine_latest/1989",
          "Src/TropicalMedicine_latest/1990",
          "Src/TropicalMedicine_latest/1991",
          "Src/TropicalMedicine_latest/1992",
          "Src/TropicalMedicine_latest/1993",
          "Src/TropicalMedicine_latest/1994",
          "Src/TropicalMedicine_latest/1995",
          "Src/TropicalMedicine_latest/1996",
          "Src/TropicalMedicine_latest/1997",
          "Src/TropicalMedicine_latest/1998",
          "Src/TropicalMedicine_latest/1999",
          "Src/TropicalMedicine_latest/2000",
          "Src/TropicalMedicine_latest/2001",
          "Src/TropicalMedicine_latest/2002",
          "Src/TropicalMedicine_latest/2003",
          "Src/TropicalMedicine_latest/2004",
          "Src/TropicalMedicine_latest/2005",
          "Src/TropicalMedicine_latest/2006",
          "Src/TropicalMedicine_latest/2007",
          "Src/TropicalMedicine_latest/2008",
          "Src/TropicalMedicine_latest/2009",
          "Src/TropicalMedicine_latest/2010",
          "Src/TropicalMedicine_latest/2011",
          "Src/TropicalMedicine_latest/2012",
          "Src/TropicalMedicine_latest/2013",
          "Src/TropicalMedicine_latest/2014",
          "Src/TropicalMedicine_latest/2015",
          "Src/TropicalMedicine_latest/2016",
          "Src/TropicalMedicine_latest/2017",
          "Src/TropicalMedicine_latest/2018",
          "Src/TropicalMedicine_latest/2019",
          "Src/TropicalMedicine_latest/2020",
          "Src/TropicalMedicine_latest/2021")
for (i in folders){
load(paste0(i,"_df1_webofscience.RData"))
DOImissing=which(data.frame$DI=="" | is.na(data.frame$DI))
print(paste0(i,"&",length(DOImissing)))
}

# 5. Output ----

#save(data.frame,file=paste0(i,"_df1_webofscience.RData"))

