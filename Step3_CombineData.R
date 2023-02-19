# 1. Data Importation----
article.data=NULL
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
# 2. Data combination ----

for(i in folders){
  
  # Load in original WoS data frame from step 1...
  load(paste0(i,"_df1_webofscience.RData"))
  # and new crossref names from step 2
  load(paste0(i,"_df2_missingnames.RData"))
  
  # Separate out author names and find entries with initials
  all_auth_names=lapply(as.list(data.frame$AF),strsplit,split="; ")
  first_names=pbmclapply(1:length(all_auth_names),get.all.given,
                         authlist=all_auth_names,mc.cores=2)
  initials=unlist(lapply(first_names,is.initials))
  
  # Replace entries with initials with the new names you got from crossref
  data.frame$AF[initials==T]=new.names$AF
  
  all_auth_names=lapply(as.list(data.frame$AF),strsplit,split="; ")
  first_names=pbmclapply(1:length(all_auth_names),get.all.given,
                         authlist=all_auth_names,mc.cores=2)
  initials=unlist(lapply(first_names,is.initials))
  print(summary(initials))
  
  # Append this new data to the full dataset
  article.data=rbind(article.data,data.frame)
}

# 3. Output -----
# Save out full dataset with info from all journals
save(article.data, file="df3_articledata.RData")
