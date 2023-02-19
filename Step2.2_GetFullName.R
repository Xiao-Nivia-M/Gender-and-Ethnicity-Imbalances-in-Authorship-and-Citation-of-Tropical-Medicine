# 1. Library Importation----
library(pbmcapply)
library(rvest)
library(RJSONIO)
library(textclean)

# 2. Data Importation----
#import the data set 
j="Src/TropicalMedicine_latest/1950"#done
j="Src/TropicalMedicine_latest/1951"#done
j="Src/TropicalMedicine_latest/1952"#done
j="Src/TropicalMedicine_latest/1953"#done
j="Src/TropicalMedicine_latest/1954"#done
j="Src/TropicalMedicine_latest/1955"#done
j="Src/TropicalMedicine_latest/1956"#done
j="Src/TropicalMedicine_latest/1957"#done
j="Src/TropicalMedicine_latest/1958"#done
j="Src/TropicalMedicine_latest/1959"#done
j="Src/TropicalMedicine_latest/1960"#done
j="Src/TropicalMedicine_latest/1961"#done
j="Src/TropicalMedicine_latest/1962"#done
j="Src/TropicalMedicine_latest/1963"#done
j="Src/TropicalMedicine_latest/1964"#done
j="Src/TropicalMedicine_latest/1965"#done
j="Src/TropicalMedicine_latest/1966"#done
j="Src/TropicalMedicine_latest/1967"#done
j="Src/TropicalMedicine_latest/1968"#done
j="Src/TropicalMedicine_latest/1969"#done
j="Src/TropicalMedicine_latest/1970"#done
j="Src/TropicalMedicine_latest/1971"#done
j="Src/TropicalMedicine_latest/1972"#done
j="Src/TropicalMedicine_latest/1973"#done
j="Src/TropicalMedicine_latest/1974"#done
j="Src/TropicalMedicine_latest/1975"#done
j="Src/TropicalMedicine_latest/1976"#done
j="Src/TropicalMedicine_latest/1977"#done
j="Src/TropicalMedicine_latest/1978"#done
j="Src/TropicalMedicine_latest/1979"#done
j="Src/TropicalMedicine_latest/1980"#done
j="Src/TropicalMedicine_latest/1981"#done
j="Src/TropicalMedicine_latest/1982"#done
j="Src/TropicalMedicine_latest/1983"#done
j="Src/TropicalMedicine_latest/1984"#done
j="Src/TropicalMedicine_latest/1985"#done
j="Src/TropicalMedicine_latest/1986"#done
j="Src/TropicalMedicine_latest/1987"#done
j="Src/TropicalMedicine_latest/1988"#done
j="Src/TropicalMedicine_latest/1989"#done
j="Src/TropicalMedicine_latest/1990"#done
j="Src/TropicalMedicine_latest/1991"#done
j="Src/TropicalMedicine_latest/1992"#done
j="Src/TropicalMedicine_latest/1993"#done
j="Src/TropicalMedicine_latest/1994"#done
j="Src/TropicalMedicine_latest/1995"#done
j="Src/TropicalMedicine_latest/1996"#done
j="Src/TropicalMedicine_latest/1997"#done
j="Src/TropicalMedicine_latest/1998"#done
j="Src/TropicalMedicine_latest/1999"#done
j="Src/TropicalMedicine_latest/2000"#done
j="Src/TropicalMedicine_latest/2001"#done
j="Src/TropicalMedicine_latest/2002"#done
j="Src/TropicalMedicine_latest/2003"#done
j="Src/TropicalMedicine_latest/2004"#done
j="Src/TropicalMedicine_latest/2005"#done
j="Src/TropicalMedicine_latest/2006"#done
j="Src/TropicalMedicine_latest/2007"#done
j="Src/TropicalMedicine_latest/2008"#done
j="Src/TropicalMedicine_latest/2009"#done
j="Src/TropicalMedicine_latest/2010"#done
j="Src/TropicalMedicine_latest/2011"#done
j="Src/TropicalMedicine_latest/2012"#done
j="Src/TropicalMedicine_latest/2013"#done
j="Src/TropicalMedicine_latest/2014"#done
j="Src/TropicalMedicine_latest/2015"#done
j="Src/TropicalMedicine_latest/2016"#done
j="Src/TropicalMedicine_latest/2017"#done
j="Src/TropicalMedicine_latest/2018"#done
j="Src/TropicalMedicine_latest/2019"#done
j="Src/TropicalMedicine_latest/2020"#done
j="Src/TropicalMedicine_latest/2021"#done

# 3. Prepare name extraction from CrossRef ----
load(paste0(j,"_df1_webofscience.RData"))
all_auth_names=lapply(as.list(data.frame$AF),strsplit,split="; ")
first_names=pbmclapply(1:length(all_auth_names),get.all.given,
                       authlist=all_auth_names,mc.cores=2)
initials=unlist(lapply(first_names,is.initials))

needed_dois=data.frame$DI[initials==T]
needed_names=all_auth_names[initials==T]

base_url="https://api.crossref.org/v1/works/http://dx.doi.org/"
urls=paste0(base_url,needed_dois)

if(paste0(j,"_df2_missingnames.RData")%in%list.files()){
  load(paste0(j,"_df2_missingnames.RData"))
}else{
  new.names=data.frame(DI=needed_dois,
                       AF=data.frame$AF[initials==T],
                       done=rep(0,length(needed_dois)))
  new.names$AF=as.character(new.names$AF)
  new.names$DI=as.character(new.names$DI)
}

# 4. Determine missing names to be pulled from CrossRef -----
still.to.do=which(new.names$done==0)
for(i in still.to.do){
  if(!is.na(new.names$DI[i])){
  # For each article, get original names and total number of authors
  orig_author_names=new.names$AF[i]
  num_authors=length(needed_names[[i]][[1]])
  
  # Pull data from crossref for article i
  json_file=urls[i]
  json_data=try(RJSONIO::fromJSON(json_file),silent=T)
  
  # If the pull request works...
  if(class(json_data)!="try-error"){
    
    # And if there is actually author data...
    if(!is.null(json_data$message$author)){
      
      # Get their names from the resulting data pull
      crossref=get.cr.auths(json_data$message$author)
      
      # If crossref has data for all authors 
      # (sometimes they only have the first author for some reason),
      # and if they are full names and not just initials...
      if(length(crossref$firsts)==num_authors & 
         !identical(crossref$firsts,toupper(crossref$firsts))){
        
        # Replace the WoS names with the crossref names in new data frame
        new.names$AF[i]=crossref$all
        print("changed by crossref")
        print(orig_author_names)
        print(new.names$AF[i])
        
      }else{
        print("only initials")
        print(orig_author_names)
        print(crossref$all)
      }
      
     
    }else{
      
      # If crossref didn't have author data, say that
      print("Couldn't find authors")
      # Output comparison of original WoS names and new names (if desired)
      print(orig_author_names)
      print(new.names$AF[i])
    }
  }else{
    
    # If crossref couldn't find the relevant DOI, say that
    print("Couldn't find DOI")
  }
  
  # Save interim file with updated data
  save(new.names,file=paste0(j,"_df2_missingnames.RData"))
  # Make note that you completed the pull for this article
  new.names$done[i]=1
  cat(i,"of",nrow(new.names),"\n")
  
  
  # Pause to space out pull requests
  time=round(runif(1,1,3),0)
  for(t in time:1){
    Sys.sleep(1)
    cat("Countdown:",t,"\n")
  }
  }
}

# 5. Check the original files -----
all_auth_names=lapply(as.list(new.names$AF),strsplit,split="; ")
first_names=pbmclapply(1:length(all_auth_names),get.all.given,
                       authlist=all_auth_names,mc.cores=2)
initials=unlist(lapply(first_names,is.initials))
summary(initials)
j

# 6 Output -----
#save(new.names,file=paste0(j,"_df2_missingnames.RData"))