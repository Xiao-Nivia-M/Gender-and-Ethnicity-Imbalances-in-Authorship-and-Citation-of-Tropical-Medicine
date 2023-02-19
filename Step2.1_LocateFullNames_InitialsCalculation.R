# 1. Library Importation----
library(pbmcapply);
library(rvest)
library(RJSONIO);
library(textclean)

# 2. Data Preparation----
#Find out how many papers only have initials 
i="Src/TropicalMedicine_latest/1945"
i="Src/TropicalMedicine_latest/1946"
i="Src/TropicalMedicine_latest/1947"
i="Src/TropicalMedicine_latest/1948"
i="Src/TropicalMedicine_latest/1949"
i="Src/TropicalMedicine_latest/1950"
i="Src/TropicalMedicine_latest/1951"
i="Src/TropicalMedicine_latest/1952"
i="Src/TropicalMedicine_latest/1953"
i="Src/TropicalMedicine_latest/1954"
i="Src/TropicalMedicine_latest/1955"
i="Src/TropicalMedicine_latest/1956"
i="Src/TropicalMedicine_latest/1957"
i="Src/TropicalMedicine_latest/1958"
i="Src/TropicalMedicine_latest/1959"
i="Src/TropicalMedicine_latest/1960"
i="Src/TropicalMedicine_latest/1961"
i="Src/TropicalMedicine_latest/1962"
i="Src/TropicalMedicine_latest/1963"
i="Src/TropicalMedicine_latest/1964"
i="Src/TropicalMedicine_latest/1965"
i="Src/TropicalMedicine_latest/1966"
i="Src/TropicalMedicine_latest/1967"
i="Src/TropicalMedicine_latest/1968"
i="Src/TropicalMedicine_latest/1969"
i="Src/TropicalMedicine_latest/1970"
i="Src/TropicalMedicine_latest/1971"
i="Src/TropicalMedicine_latest/1972"
i="Src/TropicalMedicine_latest/1973"
i="Src/TropicalMedicine_latest/1974"
i="Src/TropicalMedicine_latest/1975"
i="Src/TropicalMedicine_latest/1976"
i="Src/TropicalMedicine_latest/1977"
i="Src/TropicalMedicine_latest/1978"
i="Src/TropicalMedicine_latest/1979"
i="Src/TropicalMedicine_latest/1980"
i="Src/TropicalMedicine_latest/1981"
i="Src/TropicalMedicine_latest/1982"
i="Src/TropicalMedicine_latest/1983"
i="Src/TropicalMedicine_latest/1984"
i="Src/TropicalMedicine_latest/1985"
i="Src/TropicalMedicine_latest/1986"
i="Src/TropicalMedicine_latest/1987"
i="Src/TropicalMedicine_latest/1988"
i="Src/TropicalMedicine_latest/1989"
i="Src/TropicalMedicine_latest/1990"
i="Src/TropicalMedicine_latest/1991"
i="Src/TropicalMedicine_latest/1992"
i="Src/TropicalMedicine_latest/1993"
i="Src/TropicalMedicine_latest/1994"
i="Src/TropicalMedicine_latest/1995"
i="Src/TropicalMedicine_latest/1996"
i="Src/TropicalMedicine_latest/1997"
i="Src/TropicalMedicine_latest/1998"
i="Src/TropicalMedicine_latest/1999"
i="Src/TropicalMedicine_latest/2000"
i="Src/TropicalMedicine_latest/2001"
i="Src/TropicalMedicine_latest/2002"
i="Src/TropicalMedicine_latest/2003"
i="Src/TropicalMedicine_latest/2004"
i="Src/TropicalMedicine_latest/2005"
i="Src/TropicalMedicine_latest/2006"
i="Src/TropicalMedicine_latest/2007"
i="Src/TropicalMedicine_latest/2008"
i="Src/TropicalMedicine_latest/2009"
i="Src/TropicalMedicine_latest/2010"
i="Src/TropicalMedicine_latest/2011"
i="Src/TropicalMedicine_latest/2012"
i="Src/TropicalMedicine_latest/2013"
i="Src/TropicalMedicine_latest/2014"
i="Src/TropicalMedicine_latest/2015"
i="Src/TropicalMedicine_latest/2016"
i="Src/TropicalMedicine_latest/2017"
i="Src/TropicalMedicine_latest/2018"
i="Src/TropicalMedicine_latest/2019"
i="Src/TropicalMedicine_latest/2020"
i="Src/TropicalMedicine_latest/2021"


load(paste0(i,"_df1_webofscience.RData"))
all_auth_names=lapply(as.list(data.frame$AF),strsplit,split="; ")
first_names=pbmclapply(1:length(all_auth_names),get.all.given,
                       authlist=all_auth_names,mc.cores=2)

# 3. Check the initials status -----
#View(first_names)
initials=unlist(lapply(first_names,is.initials))
summary(initials)


# 4. Recheck the result form step 2.2-----
for(i in folders){
load(paste0(i,"_df2_missingnames.RData"))
all_auth_names=lapply(as.list(new.names$AF),strsplit,split="; ")
first_names=pbmclapply(1:length(all_auth_names),get.all.given,
                       authlist=all_auth_names,mc.cores=2)
#View(first_names)
initials=unlist(lapply(first_names,is.initials))
print(summary(initials))
}


