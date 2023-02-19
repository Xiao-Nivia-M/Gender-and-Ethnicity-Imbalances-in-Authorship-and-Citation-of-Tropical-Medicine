library(rjson)
library(pbmcapply)
library(stringr)
load("df5_articledata_matchednames.RData")
cores=detectCores()

all_auth_names=lapply(as.list(article.data$AF),strsplit,split="; ")

# Isolate first- and last-authors' names
first_last_auths_race=pbmclapply(all_auth_names,get.first.last.race,mc.cores=cores)

# Get unique first names for race estimation
name_list_race=unique(unlist(first_last_auths_race))
initials=unlist(lapply(name_list_race,is.initials))


# Create dataset for names and predicted race
nameraces=data.frame(name=name_list_race,
                     originalcountry=rep(NA,length(name_list_race)),
                     propabilities=rep(NA,length(name_list_race)),
                     initials=rep(NA,length(name_list_race)))
# Make name variable chr type
nameraces$name=as.character(nameraces$name)

# Insert "-1" for initials to make a remark
nameraces$initials[initials==T]=-1

save(nameraces,file="df6_nameraces.RData")

# Enter NamePrism API Token
nameprism_api_key="beb3187329c28e9d"

# Determine which names have yet to be queried from gender-api
r=which(is.na(nameraces$originalcountry))
length(r)
for (i in r){
this_name=strsplit(nameraces$name[i],",")
this_name=paste0(this_name[[1]][2],"%20",this_name[[1]][1])
this_name=gsub(" ","",this_name) 
  
# Pull json data from NamePrism API
json_file=paste0("https://www.name-prism.com/api_token/nat/json/",nameprism_api_key,
                 "/",this_name)
json_data=fromJSON(file=json_file)
json_data=as.data.frame(json_data)
nameraces$originalcountry[i]=colnames(json_data)[apply(json_data,1,which.max)]
nameraces$propabilities[i]=max(json_data)

# Output the name and associated race probability (optional)
print(paste0(i,"/",length(r)," ",nameraces$name[i],"=",nameraces$originalcountry[i],
             ": confidence=",nameraces$propabilities[i]))

# Save the interim file so you can pick back up later if you decide to stop
save(nameraces,file="df6_nameraces.RData")

# Pause to space out pull requests
time=round(runif(1,1,3),0)
for(t in time:1){
  Sys.sleep(1)
  cat("Countdown:",t,"\n")
}
}

