#1. Library Imporation -----
library(rjson)
library(pbmcapply)
library(stringr)

# 2. Data Importation -----
# Load in gender data set from step 6
load("df6_namegends.RData")

# Load in article data set from step 5
load("df5_articledata_matchednames.RData")

# Save number of cores on machine
cores=detectCores()

#3. Isolate first names of first- and last-authors----
all_auth_names=lapply(as.list(article.data$AF),strsplit,split="; ")
first_names=pbmclapply(1:length(all_auth_names),get.all.given,
                       authlist=all_auth_names,mc.cores=cores)
first_last_auths=pbmclapply(first_names,get.first.last,mc.cores=cores)

#4. Assign probabilistic genders to author names-----
# 'Threshold' gives the probability above which you will assign a given gender
# This returns combinations of "M"=man, "W"=woman, and "U"=unknown
# giving e.g., "MW", "WM", "WU", "UU", etc. for each article
article_auth_gends=pbmclapply(first_last_auths,gend.to.auths,
                              namegends,threshold=0.7)

#5. Create new variable in article.data that gives author gender category-----
article.data$AG=unlist(article_auth_gends)

# 6. Result -----
#6.1 See proportion of articles for which gender could be assigned to both first and last author
table(!grepl("U",article.data$AG))/nrow(article.data)

# Save new article data with gender categories
save(article.data, file="df7_articledata_withgenders.RData")
