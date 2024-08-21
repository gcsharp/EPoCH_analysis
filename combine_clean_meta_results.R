
require(tidyverse)
filenames <- list.files("/Volumes/MRC-IEU-research/projects/ieu2/p5/015/working/data/meta_analysis_results/")
filenames <- filenames[grep("extracted",filenames)]
#filenames <- filenames[-grep("1c|2c|3c|4c",filenames)]
filenames <- paste0("/Volumes/MRC-IEU-research/projects/ieu2/p5/015/working/data/meta_analysis_results/",filenames)

files <- lapply(filenames,readRDS)

combined <- bind_rows(files)

# save
saveRDS(combined,"~/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofBristol/grp-EPoCH - Documents/EPoCH GitHub/all_results.rds") #"~/Library/CloudStorage/OneDrive-SharedLibraries-UniversityofBristol/grp-EPoCH - Documents/EPoCH GitHub/all_results.rds"
saveRDS(combined,"~/University of Bristol/grp-EPoCH - Documents/EPoCH GitHub/all_results.rds") 
saveRDS(combined,"~/OneDrive - University of Exeter/Projects/EPoCH/all_results.rds")

# # add bib_sa and bib_we results REMOVE THIS AS BIB WE AND SE ARE NOT SHOWING DIFFERENT CONFOUNDING STRUCTURES
# 
# bib_sa_filenames <- list.files("/Volumes/MRC-IEU-research/projects/ieu2/p5/015/working/data/bibsa/results/",full.names = T)
# bib_sa_filenames <- bib_sa_filenames[grep("phewas",bib_sa_filenames)]
# bib_we_filenames <- list.files("/Volumes/MRC-IEU-research/projects/ieu2/p5/015/working/data/bibwe/results/",full.names = T)
# bib_we_filenames <- bib_we_filenames[grep("phewas",bib_we_filenames)]
# 
# bib_sa<- lapply(bib_sa_filenames,readRDS)
# bib_we<- lapply(bib_we_filenames,readRDS)
# bib_sa <- bind_rows(bib_sa)
# bib_we <- bind_rows(bib_we)
# 
# bib_we_key <- readRDS("/Volumes/MRC-IEU-research/projects/ieu2/p5/015/working/data/bibwe/bibwe_key.rds")
# bib_sa_key <- readRDS("/Volumes/MRC-IEU-research/projects/ieu2/p5/015/working/data/bibsa/bibsa_key.rds")
# 
# merge_key_res <- function(key,res,cohort){
#   res <- res[,c("exposure","regression_term","outcome","est","se","p","n","exposure_n","outcome_n","model")]
#   res <- res[which(res$regression_term!="error"),] # can remove this once we have sorted the issue with MCS
#   res$cohort <- cohort
#   #just tidying up a bit due to (accidental) differences in the make_key process for MoBa and the other cohorts - but actually this column isn't needed for the meta-analysis
#   key$exposure_source <-"reported by self or study mother"
#   key$exposure_linker<-str_replace(key$exposure_linker,pattern="self-reported|reported by self or study mother|self-reported or measured","reported by self or study mother")
#   res <- merge(res,key,by=c("exposure","outcome"),all.y=F)
#   res$exposure_dose <-NA
#   res$exposure_dose[grep("Heavy",res$regression_term)]<-"heavy"
#   res$exposure_dose[grep("Light",res$regression_term)]<-"light"
#   res$exposure_dose[grep("Moderate",res$regression_term)]<-"moderate"
#   res$exposure_linker <- paste(res$exposure_linker,res$exposure_dose)
#   res$exposure <-NULL
#   res$regression_term <-NULL
#   res<-res[,-grep(colnames(res),pattern="covariate")]
#   res
# }
# 
# bib_sa_merged <- merge_key_res(bib_sa_key,bib_sa,"BIBSA")
# bib_we_merged <- merge_key_res(bib_we_key,bib_we,"BIBWE")
# 
# bib_stratified <- left_join(bib_sa_merged,bib_we_merged,by=c("exposure_linker","outcome_linker","exposure_class","exposure_subclass","exposure_time","exposure_type","exposure_source","person_exposed","outcome_class","outcome_subclass1","outcome_subclass2","outcome_time","outcome_type","exposure_dose","model"),suffix = c("_BIBSA","_BIBWE"))  
# 
# # remove those with BIB stratified n<20
# KEEP <- rep(TRUE,nrow(bib_stratified))
# KEEP[as.numeric(bib_stratified$n_BIBSA)<20]<-FALSE
# KEEP[as.numeric(bib_stratified$n_BIBWE)<20]<-FALSE
# 
# # or any exposed n <5
# KEEP[as.numeric(bib_stratified$exposure_n_BIBSA)<5]<-FALSE
# KEEP[as.numeric(bib_stratified$exposure_n_BIBWE)<5]<-FALSE
# 
# # or any unexposed n <5
# KEEP[(as.numeric(bib_stratified$n_BIBSA) - as.numeric(bib_stratified$exposure_n_BIBSA))<5]<-FALSE
# KEEP[(as.numeric(bib_stratified$n_BIBWE) - as.numeric(bib_stratified$exposure_n_BIBWE))<5]<-FALSE
# 
# # or those with outcome n <5
# KEEP[as.numeric(bib_stratified$outcome_n_BIBSA)<5]<-FALSE
# KEEP[as.numeric(bib_stratified$outcome_n_BIBWE)<5]<-FALSE
# 
# # or those without outcome n <5
# KEEP[(as.numeric(bib_stratified$n_BIBSA) - as.numeric(bib_stratified$outcome_n_BIBSA))<5]<-FALSE
# KEEP[(as.numeric(bib_stratified$n_BIBWE) - as.numeric(bib_stratified$outcome_n_BIBWE))<5]<-FALSE
# 
# bib_stratified <- bib_stratified[KEEP,]
# 
# bib_stratified <- bib_stratified[,c("exposure_linker","outcome_linker","model","est_BIBSA","se_BIBSA","p_BIBSA","n_BIBSA","exposure_n_BIBSA","outcome_n_BIBSA",
#                                     "est_BIBWE","se_BIBWE","p_BIBWE","n_BIBWE","exposure_n_BIBWE","outcome_n_BIBWE")]
# 
# combined <- merge(combined, bib_stratified, by=c("exposure_linker","outcome_linker","model"),all.x=T)

#remove if all results and sample size ==NA

#combined <- combined[-which(combined$total_n==0),]
combined_cleaned <- combined

# # remove results for model 3 if exposure is not time sensitive UPDATE THIS IF DECIDE TO KEEP FIRST TRIMESTER ADJ FOR PRECONCEPTION
# combined_cleaned <- combined[-which(
#     grepl(combined$model,pattern="3") 
#     & combined$exposure_time %in% c("ever in life","age at initiation","alcohol","at study recruitment","caffeine","cessation","early onset","ever in pregnancy","heaviness","initiation","physical activity","preconception","first trimester")
# ),]

# remove results for model 2 and 3 if exposure is a genetic risk score
combined_cleaned <- combined_cleaned[-which(
  grepl(combined_cleaned$model,pattern="2|3")
  & combined_cleaned$exposure_subclass=="polygenic risk score"
),]

# remove age and sex adjusted BMI  as it's confusing and the results are similar to non-adjusted (and we adjust for age and sex in the model anyway)
#combined_cleaned <- combined_cleaned[-which(combined_cleaned$outcome_subclass2=="age and sex adjusted BMI"),]
# remove results related to physical activity or diet (for app)
combined_cleaned <- combined_cleaned[-which(combined_cleaned$exposure_class %in% c("physical activity","diet")),]
# and results for perinatal survival outcomes
combined_cleaned <- combined_cleaned[-which(combined_cleaned$outcome_class %in% c("perinatal survival")),]
# and negative control outcomes
combined_cleaned <- combined_cleaned[-which(combined_cleaned$outcome_class %in% c("negative control outcomes")),]


#and a bit more tidying to remove ordinal ever-in-pregnancy exposures that I made accidentally:
combined_cleaned <- combined_cleaned[-which((combined_cleaned$exposure_type=="ordinal"|combined_cleaned$exposure_type=="continuous")&
                                              (combined_cleaned$exposure_time=="ever in pregnancy")),]

# change terminology of prs to grs
combined_cleaned[] <- lapply(combined_cleaned, gsub, pattern = "polygenic risk score", replacement = "genetic risk score", fixed = TRUE)


# and then change name of model 1 to model 2 if grs:
combined_cleaned$model[combined_cleaned$exposure_subclass=="genetic risk score"&combined_cleaned$model=="model1a"]<-"model2a"
combined_cleaned$model[combined_cleaned$exposure_subclass=="genetic risk score"&combined_cleaned$model=="model1a_FEMALE"]<-"model2a_FEMALE"
combined_cleaned$model[combined_cleaned$exposure_subclass=="genetic risk score"&combined_cleaned$model=="model1a_MALE"]<-"model2a_MALE"
combined_cleaned$model[combined_cleaned$exposure_subclass=="genetic risk score"&combined_cleaned$model=="model1b"]<-"model2b"
combined_cleaned$model[combined_cleaned$exposure_subclass=="genetic risk score"&combined_cleaned$model=="model1b_FEMALE"]<-"model2b_FEMALE"
combined_cleaned$model[combined_cleaned$exposure_subclass=="genetic risk score"&combined_cleaned$model=="model1b_MALE"]<-"model2b_MALE"

# remove NA from end of exposure linker
combined_cleaned[] <- lapply(combined_cleaned, gsub, pattern = " NA", replacement = "", fixed = TRUE)

# change terminology of psychosocial traits to above clinical threshold for binary traits

change_terminology <- function(oldterm,datatype,newterm){
  combined_cleaned[which(combined_cleaned$outcome_subclass2==oldterm&combined_cleaned$outcome_type==datatype),] <- 
    apply(combined_cleaned[which(combined_cleaned$outcome_subclass2==oldterm&combined_cleaned$outcome_type==datatype),],2, gsub, pattern = oldterm, replacement = newterm, fixed = TRUE)
  combined_cleaned
}

combined_cleaned <- change_terminology("BMI >25","binary","overweight or obese")
combined_cleaned <- change_terminology("BMI >30","binary","obese")
combined_cleaned <- change_terminology("autism","continuous","risk for ASD") ## can remove this one once this is changed in the original data
combined_cleaned <- change_terminology("autism","binary","risk for ASD > CT")
combined_cleaned <- change_terminology("total difficulties (SDQ)","binary","total difficulties (SDQ) >=17")
combined_cleaned <- change_terminology("prosocial behaviour","binary","prosocial behaviour <=4")
combined_cleaned <- change_terminology("peer relationship problems","binary","peer relationship problems >=4")
combined_cleaned <- change_terminology("hyperactivity/inattention","binary","hyperactivity/inattention >=7")
combined_cleaned <- change_terminology("emotional symptoms","binary","emotional symptoms >=5")
combined_cleaned <- change_terminology("conduct problems","binary","conduct problems >=4")

combined_cleaned <- change_terminology("behaviour and affect","continuous","depressive symptoms")
combined_cleaned$outcome_subclass1[combined_cleaned$outcome_subclass2=="depressive symptoms"]<-"behaviour and affect"

# change terminology of behaviour and affect
combined_cleaned[] <- lapply(combined_cleaned, gsub, pattern = "behaviour and affect", replacement = "behaviour and emotions", fixed = TRUE)

# change terminology of SEP and SEP 'dose' (change dose to level?)
combined_cleaned[] <- lapply(combined_cleaned, gsub, pattern = "socioeconomic position", replacement = "low socioeconomic position", fixed = TRUE)
combined_cleaned[] <- lapply(combined_cleaned, gsub, pattern = "highest education", replacement = "low SEP (education)", fixed = TRUE)
combined_cleaned[] <- lapply(combined_cleaned, gsub, pattern = "occupation", replacement = "low SEP (occupation)", fixed = TRUE)

# add new model names
combined_cleaned$model_new_name <- NA
combined_cleaned$model_new_name[combined_cleaned$model=="model1a"]<-"Minimal adjustment - without co-parent's exposure"
combined_cleaned$model_new_name[combined_cleaned$model=="model1b"]<-"Minimal adjustment - with co-parent's exposure"
combined_cleaned$model_new_name[combined_cleaned$model=="model2a"]<-"Standard adjustment - without co-parent's exposure"
combined_cleaned$model_new_name[combined_cleaned$model=="model2b"]<-"Standard adjustment - with co-parent's exposure"
combined_cleaned$model_new_name[combined_cleaned$model=="model3a"]<-"Standard adjustment - without co-parent's exposure - plus exposure at previous timepoint(s)"
combined_cleaned$model_new_name[combined_cleaned$model=="model3b"]<-"Standard adjustment - with co-parent's exposure - plus exposure at previous timepoint(s)"
combined_cleaned$model_new_name[combined_cleaned$model=="model4a"]<-"Standard adjustment - without co-parent's exposure - plus potential mediators"
combined_cleaned$model_new_name[combined_cleaned$model=="model4b"]<-"Standard adjustment - with co-parent's exposure - plus potential mediators"

combined_cleaned$model_new_name[combined_cleaned$model=="model1a_FEMALE"]<-"Sex stratified FEMALE: Minimal adjustment - without co-parent's exposure"
combined_cleaned$model_new_name[combined_cleaned$model=="model1b_FEMALE"]<-"Sex stratified FEMALE: Minimal adjustment - with co-parent's exposure"
combined_cleaned$model_new_name[combined_cleaned$model=="model2a_FEMALE"]<-"Sex stratified FEMALE: Standard adjustment - without co-parent's exposure"
combined_cleaned$model_new_name[combined_cleaned$model=="model2b_FEMALE"]<-"Sex stratified FEMALE: Standard adjustment - with co-parent's exposure"
combined_cleaned$model_new_name[combined_cleaned$model=="model3a_FEMALE"]<-"Sex stratified FEMALE: Standard adjustment - without co-parent's exposure - plus exposure at previous timepoint(s)"
combined_cleaned$model_new_name[combined_cleaned$model=="model3b_FEMALE"]<-"Sex stratified FEMALE: Standard adjustment - with co-parent's exposure - plus exposure at previous timepoint(s)"
combined_cleaned$model_new_name[combined_cleaned$model=="model4a_FEMALE"]<-"Sex stratified FEMALE: Standard adjustment - without co-parent's exposure - plus potential mediators"
combined_cleaned$model_new_name[combined_cleaned$model=="model4b_FEMALE"]<-"Sex stratified FEMALE: Standard adjustment - with co-parent's exposure - plus potential mediators"

combined_cleaned$model_new_name[combined_cleaned$model=="model1a_MALE"]<-"Sex stratified MALE: Minimal adjustment - without co-parent's exposure"
combined_cleaned$model_new_name[combined_cleaned$model=="model1b_MALE"]<-"Sex stratified MALE: Minimal adjustment - with co-parent's exposure"
combined_cleaned$model_new_name[combined_cleaned$model=="model2a_MALE"]<-"Sex stratified MALE: Standard adjustment - without co-parent's exposure"
combined_cleaned$model_new_name[combined_cleaned$model=="model2b_MALE"]<-"Sex stratified MALE: Standard adjustment - with co-parent's exposure"
combined_cleaned$model_new_name[combined_cleaned$model=="model3a_MALE"]<-"Sex stratified MALE: Standard adjustment - without co-parent's exposure - plus exposure at previous timepoint(s)"
combined_cleaned$model_new_name[combined_cleaned$model=="model3b_MALE"]<-"Sex stratified MALE: Standard adjustment - with co-parent's exposure - plus exposure at previous timepoint(s)"
combined_cleaned$model_new_name[combined_cleaned$model=="model4a_MALE"]<-"Sex stratified MALE: Standard adjustment - without co-parent's exposure - plus potential mediators"
combined_cleaned$model_new_name[combined_cleaned$model=="model4b_MALE"]<-"Sex stratified MALE: Standard adjustment - with co-parent's exposure - plus potential mediators"

combined_cleaned$model_new_name[combined_cleaned$model=="model2a"&combined_cleaned$exposure_subclass=="genetic risk score"]<-"Standard adjustment - without co-parent's GRS"
combined_cleaned$model_new_name[combined_cleaned$model=="model2b"&combined_cleaned$exposure_subclass=="genetic risk score"]<-"Standard adjustment - with co-parent's GRS"
combined_cleaned$model_new_name[combined_cleaned$model=="model4a"&combined_cleaned$exposure_subclass=="genetic risk score"]<-"Standard adjustment - without co-parent's GRS - plus child's GRS"
combined_cleaned$model_new_name[combined_cleaned$model=="model4b"&combined_cleaned$exposure_subclass=="genetic risk score"]<-"Standard adjustment - with co-parent's GRS - plus child's GRS"

combined_cleaned$model_new_name[combined_cleaned$model=="model2a_MALE"&combined_cleaned$exposure_subclass=="genetic risk score"]<-"Sex stratified MALE: Standard adjustment - without co-parent's GRS"
combined_cleaned$model_new_name[combined_cleaned$model=="model2b_MALE"&combined_cleaned$exposure_subclass=="genetic risk score"]<-"Sex stratified MALE: Standard adjustment - with co-parent's GRS"
combined_cleaned$model_new_name[combined_cleaned$model=="model4a_MALE"&combined_cleaned$exposure_subclass=="genetic risk score"]<-"Sex stratified MALE: Standard adjustment - without co-parent's GRS - plus child's GRS"
combined_cleaned$model_new_name[combined_cleaned$model=="model4b_MALE"&combined_cleaned$exposure_subclass=="genetic risk score"]<-"Sex stratified MALE: Standard adjustment - with co-parent's GRS - plus child's GRS"

combined_cleaned$model_new_name[combined_cleaned$model=="model2a_FEMALE"&combined_cleaned$exposure_subclass=="genetic risk score"]<-"Sex stratified FEMALE: Standard adjustment - without co-parent's GRS"
combined_cleaned$model_new_name[combined_cleaned$model=="model2b_FEMALE"&combined_cleaned$exposure_subclass=="genetic risk score"]<-"Sex stratified FEMALE: Standard adjustment - with co-parent's GRS"
combined_cleaned$model_new_name[combined_cleaned$model=="model4a_FEMALE"&combined_cleaned$exposure_subclass=="genetic risk score"]<-"Sex stratified FEMALE: Standard adjustment - without co-parent's GRS - plus child's GRS"
combined_cleaned$model_new_name[combined_cleaned$model=="model4b_FEMALE"&combined_cleaned$exposure_subclass=="genetic risk score"]<-"Sex stratified FEMALE: Standard adjustment - with co-parent's GRS - plus child's GRS"


# change numeric columns to numeric
numeric_cols <-c(c("est","se","p","q","hetp","i2","h2"),
                 colnames(combined_cleaned)[grep(colnames(combined_cleaned),pattern="est_|se_|p_A|p_B|p_M|n_A|n_B|n_M|total_n|cohorts_n|outcome_n|exposure_n")]
)
combined_cleaned[,numeric_cols] <- apply(combined_cleaned[,numeric_cols],2,as.numeric)

#tidying up
combined_cleaned$total_n_exposure[combined_cleaned$total_n_exposure==0]<-NA
combined_cleaned$total_n_outcome[combined_cleaned$total_n_outcome==0]<-NA

combined_cleaned <- combined_cleaned[is.na(combined_cleaned$est)==F,]

## NO LONGER AN ISSUE AFTER QC OF THE COHORT RESULTS INDIVIDUALLY
# QC of cohort results (remove anything where there's evidence of the model not converging, probably because a single explanatory variable (exposure or covariate), uniquely identifies the outcome, i.e. perfect prediction/complete separation.)
## evidence predicted by a very high effect estimate combined with a large P-value
## only an issue for binary outcomes - continuous estimates are within plausible range
summary(combined_cleaned$est[combined_cleaned$outcome_type=="continuous"])
summary(exp(combined_cleaned$est[combined_cleaned$outcome_type=="binary"]))
cut_off_cont <- 10
summary(abs(combined_cleaned$est[combined_cleaned$outcome_type=="continuous"])>cut_off_cont)
cut_off_bin <- 4
summary(abs(combined_cleaned$est[combined_cleaned$outcome_type=="binary"])>cut_off_bin)
  # df <- combined_cleaned
  # df_cleaned <- df[which((df$outcome_type=="binary"& abs(df$est)<=cut_off_bin)|
  #                    df$outcome_type=="continuous"), ]
  # lost_exposures <- unique(df$exposure_linker[(df$exposure_linker %in% df_cleaned$exposure_linker)==F])
  # lost_outcomes <- unique(df$outcome_linker[(df$outcome_linker %in% df_cleaned$outcome_linker)==F])
  # original_associations <-paste(df$exposure_linker,df$outcome_linker)
  # cleaned_associations <-paste(df_cleaned$exposure_linker,df_cleaned$outcome_linker)
  # lost_associations <- unique(original_associations[(original_associations %in% cleaned_associations)==F])

df_cleaned <- combined_cleaned
  
#fdr adjustment by model

  fdr_adjust_by_model <- function(modelname){
    df <- df_cleaned[df_cleaned$model==modelname,]
    df$fdr <- p.adjust(df$p,method="fdr")
    df[,c("exposure_linker","outcome_linker","model","fdr")]
  }
  
fdr_adj <-  lapply(unique(df_cleaned$model),fdr_adjust_by_model)
require(dplyr)
fdr_adj <- bind_rows(fdr_adj)
df_cleaned$fdr <-NULL
df_cleaned <- merge(df_cleaned,fdr_adj,by=c("exposure_linker","outcome_linker","model"))
  
saveRDS(df_cleaned,"~/University of Bristol/grp-EPoCH - Documents/EPoCH GitHub/all_results_reduced.rds") 
saveRDS(df_cleaned,"~/OneDrive - University of Exeter/Projects/EPoCH/EPoCH results app/data/rds/all_results_reduced.rds")



