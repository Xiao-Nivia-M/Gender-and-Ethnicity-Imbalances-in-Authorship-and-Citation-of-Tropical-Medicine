library(rjson)
library(pbmcapply)
library(readr)
library(dbplyr)

########1. Load in races data set from step 6#######
load("df6_nameraces.RData")

########2. Isolate first names of first- and last-authors#######
all_auth_names=lapply(as.list(article.data$AF),strsplit,split="; ")

columns=c("fa","la")
first_last_auths_race=data.frame(matrix(nrow=0,ncol=length(columns)))
colnames(first_last_auths_race)=columns
for(i in 1:length(all_auth_names)){
  fa=head(all_auth_names[[i]],1)
  la=tail(all_auth_names[[i]],1)
  output=c(fa,la)
  first_last_auths_race=rbind(first_last_auths_race,output)
}

###########3. Check races category and normalize countries into continent############
#According to UNSD-Methodology; Celtic English= English speaking areas
#Threshold=0.7
country.cat=data.frame(table(nameraces$originalcountry))
continent.match<- as.data.frame(read_csv("Src/MatchingTable.csv"))
colnames(continent.match)=c("originalcountry","region")
match.region=merge(x = nameraces, y =continent.match, by = "originalcountry")
match.region$propabilities=as.numeric(match.region$propabilities)

for (i in 1:nrow(match.region)){
  if(match.region$propabilities[i]>=0.7){
    match.region$AssignedRegion[i]=match.region$region[i]
  }else{
    match.region$AssignedRegion[i]="Unknown"
  }
}



###########4. Assign Races############
first_last_auths_race$fa_region=NA
first_last_auths_race$la_region=NA

for (i in 1:nrow(match.region)){
  fa_index=which(match.region$name[i]==first_last_auths_race$fa)
  first_last_auths_race$fa_region[fa_index]=match.region$AssignedRegion[i]
  la_index=which(match.region$name[i]==first_last_auths_race$la)
  first_last_auths_race$la_region[la_index]=match.region$AssignedRegion[i]
}

##########5. Find the missing races- fa##############
na.index=which(is.na(first_last_auths_race$fa_region))
missing.name=data.frame(first_last_auths_race$fa[na.index])


##########5.1 Missing races-faa##############

missing.name.list=unique(missing.name)
missing.nameraces=data.frame(name=missing.name.list$first_last_auths_race.fa.na.index.,
                     originalcountry=rep(NA,nrow(missing.name.list)),
                     propabilities=rep(NA,nrow(missing.name.list)))

save(missing.nameraces,file="df7_nameraces.missing.RData")

# Enter NamePrism API Token
nameprism_api_key="beb3187329c28e9d"

# Determine which names have yet to be queried from gender-api
r=which(is.na(missing.nameraces$originalcountry))
length(r)
for (i in r){
  this_name=strsplit(missing.nameraces$name[i],",")
  this_name=paste0(this_name[[1]][2],"%20",this_name[[1]][1])
  this_name=gsub(" ","",this_name) 
  
  # Pull json data from NamePrism API
  json_file=paste0("https://www.name-prism.com/api_token/nat/json/",nameprism_api_key,
                   "/",this_name)
  json_data=fromJSON(file=json_file)
  json_data=as.data.frame(json_data)
  missing.nameraces$originalcountry[i]=colnames(json_data)[apply(json_data,1,which.max)]
  missing.nameraces$propabilities[i]=max(json_data)
  
  # Output the name and associated race probability (optional)
  print(paste0(i,"/",length(r)," ",missing.nameraces$name[i],"=",missing.nameraces$originalcountry[i],
               ": confidence=",missing.nameraces$propabilities[i]))
  
  # Save the interim file so you can pick back up later if you decide to stop
  save(missing.nameraces,file="df7_nameraces.missing.RData")
  
  # Pause to space out pull requests
  #time=round(runif(1,1,3),0)
  #for(t in time:1){
    #Sys.sleep(1)
    #cat("Countdown:",t,"\n")
  #}
}

##########5.2 Add the missing races into assignment_fa##############
match.region.add=merge(x = missing.nameraces, y =continent.match, by = "originalcountry")
match.region.add$propabilities=as.numeric(match.region.add$propabilities)

for (i in 1:nrow(match.region.add)){
  if(match.region.add$propabilities[i]>=0.7){
    match.region.add$AssignedRegion[i]=match.region.add$region[i]
  }else{
    match.region.add$AssignedRegion[i]="Unknown"
  }
}


for (i in which(is.na(first_last_auths_race$fa_region))){
  fa_index=which(first_last_auths_race$fa[i]==match.region.add$name)
  first_last_auths_race$fa_region[i]=match.region.add$AssignedRegion[fa_index]
}

##########6. Find the missing races- la##############
na.index.la=which(is.na(first_last_auths_race$la_region))
missing.name.la=data.frame(first_last_auths_race$fa[na.index.la])


##########6.1 Missing races-la##############

missing.name.list.la=unique(missing.name.la)
missing.nameraces.la=data.frame(name=missing.name.list.la$first_last_auths_race.fa.na.index.la.,
                             originalcountry=rep(NA,nrow(missing.name.list.la)),
                             propabilities=rep(NA,nrow(missing.name.list.la)))

save(missing.nameraces.la,file="df7_nameraces.missing.la.RData")

# Enter NamePrism API Token
nameprism_api_key="beb3187329c28e9d"

# Determine which names have yet to be queried from gender-api
r=which(is.na(missing.nameraces.la$originalcountry))
length(r)
for (i in r){
  this_name=strsplit(missing.nameraces.la$name[i],",")
  this_name=paste0(this_name[[1]][2],"%20",this_name[[1]][1])
  this_name=gsub(" ","",this_name) 
  
  # Pull json data from NamePrism API
  json_file=paste0("https://www.name-prism.com/api_token/nat/json/",nameprism_api_key,
                   "/",this_name)
  json_data=fromJSON(file=json_file)
  json_data=as.data.frame(json_data)
  missing.nameraces.la$originalcountry[i]=colnames(json_data)[apply(json_data,1,which.max)]
  missing.nameraces.la$propabilities[i]=max(json_data)
  
  # Output the name and associated race probability (optional)
  print(paste0(i,"/",length(r)," ",missing.nameraces.la$name[i],"=",missing.nameraces.la$originalcountry[i],
               ": confidence=",missing.nameraces.la$propabilities[i]))
  
  # Save the interim file so you can pick back up later if you decide to stop
  save(missing.nameraces.la,file="df7_nameraces.missing.la.RData")
  
  # Pause to space out pull requests
  #time=round(runif(1,1,3),0)
  #for(t in time:1){
  #Sys.sleep(1)
  #cat("Countdown:",t,"\n")
  #}
}


##########6.2 Add the missing races into assignment_la##############
match.region.add.la=merge(x = missing.nameraces.la, y =continent.match, by = "originalcountry")
match.region.add.la$propabilities=as.numeric(match.region.add.la$propabilities)

for (i in 1:nrow(match.region.add.la)){
  if(match.region.add.la$propabilities[i]>=0.7){
    match.region.add.la$AssignedRegion[i]=match.region.add.la$region[i]
  }else{
    match.region.add.la$AssignedRegion[i]="Unknown"
  }
}


for (i in which(is.na(first_last_auths_race$la_region))){
  la_index=which(first_last_auths_race$fa[i]==match.region.add.la$name)
  first_last_auths_race$la_region[i]=match.region.add.la$AssignedRegion[la_index]
}


##########7. Assign probabilistic genders to author names##############
# 'Threshold' gives the probability above which you will assign a given race 
# This returns combinations of 


first_last_auths_race$AR=paste0(first_last_auths_race$fa_region,"+",first_last_auths_race$la_region)

article.data$AR=first_last_auths_race$AR
article.data$fa_region=first_last_auths_race$fa_region
article.data$la_region=first_last_auths_race$la_region



# Save new article data with gender categories
save(article.data, file="df7_articledata_withraces.RData")



#8.Assign race group----
string1=c("Eastern Asia","Southern Asia","South-eastern Asia","Western Asia")
string2=c("EnglishSpeaking Regions")
string3=c("Eastern Europe","Southern Europe","Wesrtern Europe","Northern Europe")
string4=c("Sub-Saharan Africa","Northern Africa")
article.data$fa_region_2=NA
article.data$la_region_2=NA

for(i in 1:nrow(article.data)){
  if(str_detect(article.data$fa_region[i],paste(string1, collapse = "|") )){
    article.data$fa_region_2[i]="Asia"
  }else if (str_detect(article.data$fa_region[i],paste(string2, collapse = "|") )){
    article.data$fa_region_2[i]="Anglophone"
  }else if(str_detect(article.data$fa_region[i],paste(string3, collapse = "|"))){
    article.data$fa_region_2[i]="EU"
  }else if(str_detect(article.data$fa_region[i],paste(string4, collapse = "|"))){
    article.data$fa_region_2[i]="Africa"
  }else{
    article.data$fa_region_2[i]=0
  }
}

for(i in 1:nrow(article.data)){
  if(str_detect(article.data$la_region[i],paste(string1, collapse = "|") )){
    article.data$la_region_2[i]="Asia"
  }else if (str_detect(article.data$la_region[i],paste(string2, collapse = "|") )){
    article.data$la_region_2[i]="Anglophone"
  }else if(str_detect(article.data$la_region[i],paste(string3, collapse = "|"))){
    article.data$la_region_2[i]="EU"
  }else if(str_detect(article.data$la_region[i],paste(string4, collapse = "|"))){
    article.data$la_region_2[i]="Africa"
  }else{
    article.data$la_region_2[i]=0
  }
}


article.data$AR_2=paste0(article.data$fa_region_2,"+",article.data$la_region_2)
for(i in 1:nrow(article.data)){
  if(str_detect(article.data$AR_2[i],"0" )){
    article.data$AR_2[i]="Unknown"
  }else{
    article.data$AR_2[i]=article.data$AR_2[i]
  }
}

table(article.data$AR_2)

save(article.data, file="df_articledata_Jan30.RData")



