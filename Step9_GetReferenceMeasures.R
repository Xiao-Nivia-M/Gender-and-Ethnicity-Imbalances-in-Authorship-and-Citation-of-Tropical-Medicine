# 1. Library Importation----
library(pbmcapply)
library(ggpubr)

options(scipen=999)

# 2. Data Importation----
# Load in data from step 8
load("df8_articledata_expecdata.RData")
load("Outcome/ResultingVariables/Step4.1_DataProcessing_TPH/df_1009_TPH_final.Rdata")

# 3. System Setting----
#3.1 Save number of cores on machine
cores=detectCores()

#3.2 Test that the function performs as expected
get.ref.props(2,article.data,uncond_expecs,cond_expecs)
for(i in sample(1:nrow(article.data),100)){
  get.ref.props(i,article.data,uncond_expecs,cond_expecs)
  print(i)
}
#4. Get the Reference Measurement ----
# 1) observed gender proportions (cols 1-4),
# 2) expected proportions under unconditional null (cols 5-8),
# 3) expected proportions under conditional null (cols 9-12), and
# 4) total number of non-self-authored candidate citations (col 13)
ref_proportions=pbmclapply(1:nrow(article.data),get.ref.props,article.data,
                           uncond_expecs,cond_expecs,mc.cores=cores)
ref_proportions=do.call(rbind,ref_proportions)


#5. Result----
# Save article data and citation proportion data
save(article.data,uncond_expecs,cond_expecs,ref_proportions,
     file="df9_articledata_propdata.RData")





