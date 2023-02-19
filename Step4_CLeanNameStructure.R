#1. Library Importation ----
library(pbmcapply)
ibrary(stringr)
library(RJSONIO)
library(textclean)

#2. Import Data ----
load("df3_articledata.RData")

#3. Find number of authors in each article ----
num.auths=unlist(lapply(article.data$AF,str_count, "; "))+1
num.commas=unlist(lapply(article.data$AF,str_count, ", "))

#4. Find articles where at least one author did not have delineated first-/last-name----
missing.comma=which(num.auths>num.commas)
r=0; num.missing=length(missing.comma)

#5. For articles with missing delineation----

for(i in missing.comma){
  
  # Pull name data from crossref to get delineated first-/last-names
  delineated.names=add.miss.comma(i,article.data$AF,article.data$DI)
  
  # Enter the newly formatted names into dataset
  # Note: gsub call just adds a space (" ") for names with no provided first name
  article.data$AF[i]=gsub(",;",", ;",delineated.names)
  
  # Iterate and provide progress update
  r=r+1; cat(r,"of",num.missing,"\n")
  
  # Pause to space out pull requests
  time=round(runif(1,1,3),0)
  for(t in time:1){
    Sys.sleep(1)
    cat("Countdown:",t,"\n")
  }
}

#6. Recheck ----
##6.1 Recalculate author counts and delineations----
num.auths=unlist(lapply(article.data$AF,str_count, "; "))+1
num.commas=unlist(lapply(article.data$AF,str_count, ", "))

##6.2 Check whether any articles are still missing first/last name delineation----
which(num.auths>num.commas)

#7. More comma delineations than authors----
extra.comma=which(num.auths<num.commas)
##7.1 Remove those extra commas to get clean first-/last-name delineation----
new.ec.names=unlist(lapply(article.data$AF[extra.comma],rm.extra.comma))
##7.2  Replace instances in data set----
article.data$AF[extra.comma]=new.ec.names

#8. Recheck ----
##8.1 Recalculate author counts and delineations----
num.auths=unlist(lapply(article.data$AF,str_count, "; "))+1
num.commas=unlist(lapply(article.data$AF,str_count, ", "))

##8.2 Check whether any articles still have an extra comma-----
which(num.auths!=num.commas)

#9. Output----
# Save cleaned up dataset
save(article.data, file="df4_articledata_cleannames.RData")
