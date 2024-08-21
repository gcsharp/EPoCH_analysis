require(dplyr)

# TABLE ONES
moba <- readRDS("/Users/gcs215/OneDrive - University of Exeter/Projects/EPoCH/EPoCH analysis/data_prep/cohorts/moba/table1_for_dat.rds")
mcs <- readRDS("~/University of Bristol/grp-EPoCH - Documents/EPoCH GitHub/data_prep/check_prepared_data/table1_for_dat_MCS.rds")
alspac <- readRDS("~/University of Bristol/grp-EPoCH - Documents/EPoCH GitHub/data_prep/check_prepared_data/table1_for_dat_ALSPAC.rds")
bib <- readRDS("~/University of Bristol/grp-EPoCH - Documents/EPoCH GitHub/data_prep/check_prepared_data/table1_for_dat_BIB.rds")

write.csv(print(moba),"/Users/gcs215/OneDrive - University of Exeter/Projects/EPoCH/moba_tableone.csv", row.names = T)
write.csv(print(mcs),"/Users/gcs215/OneDrive - University of Exeter/Projects/EPoCH/mcs_tableone.csv", row.names = T)
write.csv(print(alspac),"/Users/gcs215/OneDrive - University of Exeter/Projects/EPoCH/alspac_tableone.csv", row.names = T)
write.csv(print(bib),"/Users/gcs215/OneDrive - University of Exeter/Projects/EPoCH/bib_tableone.csv", row.names = T)

prepareTables <- function(cohort){
Cat <-bind_rows(cohort$CatTable$Overall,.id = "variable")
Cont <- as.data.frame(cohort$ContTable$Overall)
Cont$variable <- row.names(Cont)
Both <- full_join(Cat,Cont, by = intersect(colnames(Cat), colnames(Cont)))
Both
}

cohorts <- list(alspac,bib,mcs,moba)
prepared_cohorts <- lapply(cohorts,prepareTables)
names(prepared_cohorts) <- c("alspac","bib","mcs","moba")

key <- bind_rows(prepared_cohorts,.id = "cohort")
key <- key[order(key$variable,key$level),]
key <- key[duplicated(key$variable)==F,c("variable")]

write.csv(key,"/Users/gcs215/OneDrive - University of Exeter/Projects/EPoCH/tableone_key.csv")

# then prepare in excel
# and read back in

key <-read.csv("/Users/gcs215/OneDrive - University of Exeter/Projects/EPoCH/tableone_key_prepared.csv")
key <-as.data.frame(apply(key,2,tolower))
key$variable2 <- apply(key,1,function(x) {paste(x[2],x[3],x[4],x[5],sep=" - ")})


prepare_cohorts_with_key <- function(cohort){
  res <- merge(cohort,key[,c("variable","variable2")],by="variable",all.x=T)
  res$p.miss <-signif(res$p.miss,1)
  res$percent <-signif(res$percent,1)
  res$mean <-signif(res$mean,3)
  res$sd <-signif(res$sd,3)
  res$median <-signif(res$median,3)
  res$p25 <-signif(res$p25,3)
  res$p75 <-signif(res$p75,3)
  res$min <-signif(res$min,3)
  res$max <-signif(res$max,3)
  res$iqr <- apply(res[,c("p25","p75")],1, function(x){paste(x[1],x[2],sep=" to ")})
  res$range <- apply(res[,c("min","max")],1, function(x){paste(x[1],x[2],sep=" to ")})
  res$iqr[res$iqr=="NA to NA"]<-NA
  res$range[res$range=="NA to NA"]<-NA
  res <- res[,c("variable2","level","n","miss","p.miss","freq","percent","mean","sd","median","iqr","range")]
  colnames(res) <-c("variable","level","total n", "n missing","percent missing","n in level","percent in level","mean","sd","median","iqr","range")
  res <- res[-which(res$variable=="x -  -  - "),]
  res[duplicated(paste(res$variable,res$level,res$n,res$miss))==F,]
}

cohorts_with_key <- lapply(prepared_cohorts,prepare_cohorts_with_key)

write.csv(cohorts_with_key$alspac,"/Users/gcs215/Library/CloudStorage/OneDrive-UniversityofExeter/Projects/EPoCH/EPoCH results app/data/cohorts/alspac.csv",row.names = F)
write.csv(cohorts_with_key$mcs,"/Users/gcs215/Library/CloudStorage/OneDrive-UniversityofExeter/Projects/EPoCH/EPoCH results app/data/cohorts/mcs.csv",row.names = F)
write.csv(cohorts_with_key$bib,"/Users/gcs215/Library/CloudStorage/OneDrive-UniversityofExeter/Projects/EPoCH/EPoCH results app/data/cohorts/bib.csv",row.names = F)
write.csv(cohorts_with_key$moba,"/Users/gcs215/Library/CloudStorage/OneDrive-UniversityofExeter/Projects/EPoCH/EPoCH results app/data/cohorts/moba.csv",row.names = F)


