# EPoCH paper code

dat <- readRDS("~/OneDrive - University of Exeter/Projects/EPoCH/EPoCH results app/data/rds/all_results_reduced.rds")

#summarising sample sizes
unstrat <-dat[-grep(dat$model_new_name,pattern="stratified"),]
aggregate(unstrat$total_n, list(unstrat$cohorts_n), FUN=summary)
aggregate(unstrat$total_n, list(unstrat$cohorts_n), FUN=length)
summary(unstrat$total_n)

#summarising sample characteristics 
make_cohort_summary <- function(VAR_b_s,VAR_o_s,VAR_o_a,VAR_b_a,VAR_o_c,VAR_b_c,VAR_to_s,VAR_to_a,VAR_to_c,VAR_tb_s,VAR_tb_a,VAR_tb_c,VAR_passive,VAR_passive_t, VAR_binge_t,VAR_binge,VAR_educ_t,VAR_educ,VAR_occup_t,VAR_occup,cohortname){
as_b <- aggregate(VAR_b_s, list(unstrat$person_exposed[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="binary"],
                                unstrat$exposure_time[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="binary"]), FUN=max,na.rm=T)
as_o <- aggregate(VAR_o_s, list(unstrat$person_exposed[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="ordinal"],
                                unstrat$exposure_time[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="ordinal"],
                  unstrat$exposure_dose[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="ordinal"]), FUN=max,na.rm=T)

as_b_t <- aggregate(VAR_tb_s, list(unstrat$person_exposed[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="binary"],
                                   unstrat$exposure_time[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="binary"]), FUN=max,na.rm=T)
as_o_t <- aggregate(VAR_to_s, list(unstrat$person_exposed[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="ordinal"],
                                   unstrat$exposure_time[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="ordinal"],
                                   unstrat$exposure_dose[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="ordinal"]), FUN=max,na.rm=T)

aa_b <- aggregate(VAR_b_a, list(unstrat$person_exposed[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="binary"],
                                unstrat$exposure_time[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="binary"]), FUN=max,na.rm=T)
aa_o <- aggregate(VAR_o_a, list(unstrat$person_exposed[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="ordinal"],
                                unstrat$exposure_time[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="ordinal"],
                                unstrat$exposure_dose[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="ordinal"]), FUN=max,na.rm=T)

aa_b_t <- aggregate(VAR_tb_a, list(unstrat$person_exposed[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="binary"],
                                   unstrat$exposure_time[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="binary"]), FUN=max,na.rm=T)
aa_o_t <- aggregate(VAR_to_a, list(unstrat$person_exposed[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="ordinal"],
                                   unstrat$exposure_time[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="ordinal"],
                     unstrat$exposure_dose[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="ordinal"]), FUN=max,na.rm=T)

ac_b <- aggregate(VAR_b_c, list(unstrat$person_exposed[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="binary"],
                                unstrat$exposure_time[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="binary"]), FUN=max,na.rm=T)
ac_o <- aggregate(VAR_o_c, list(unstrat$person_exposed[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="ordinal"],
                                unstrat$exposure_time[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="ordinal"],
                            unstrat$exposure_dose[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="ordinal"]), FUN=max,na.rm=T)
ac_b_t <- aggregate(VAR_tb_c, list(unstrat$person_exposed[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="binary"],
                                   unstrat$exposure_time[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="binary"]), FUN=max,na.rm=T)
ac_o_t <- aggregate(VAR_to_c, list(unstrat$person_exposed[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="ordinal"],
                                   unstrat$exposure_time[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="ordinal"],
                                    unstrat$exposure_dose[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="ordinal"]), FUN=max,na.rm=T)
passive <- aggregate(VAR_passive, list(unstrat$person_exposed[unstrat$exposure_subclass=="passive smoke exposure"&unstrat$exposure_type=="binary"],
                                       unstrat$exposure_time[unstrat$exposure_subclass=="passive smoke exposure"&unstrat$exposure_type=="binary"]), FUN=max,na.rm=T)
binge <- aggregate(VAR_binge, list(unstrat$person_exposed[unstrat$exposure_subclass=="binge drinking"&unstrat$exposure_type=="binary"],
                                   unstrat$exposure_time[unstrat$exposure_subclass=="binge drinking"&unstrat$exposure_type=="binary"]), FUN=max,na.rm=T)
educ <- aggregate(VAR_educ, list(unstrat$person_exposed[unstrat$exposure_subclass=="low SEP (education)"&unstrat$exposure_type=="binary"],
                                 unstrat$exposure_time[unstrat$exposure_subclass=="low SEP (education)"&unstrat$exposure_type=="binary"]), FUN=max,na.rm=T)
occup <- aggregate(VAR_occup, list(unstrat$person_exposed[unstrat$exposure_subclass=="low SEP (occupation)"&unstrat$exposure_type=="binary"],
                                   unstrat$exposure_time[unstrat$exposure_subclass=="low SEP (occupation)"&unstrat$exposure_type=="binary"]), FUN=max,na.rm=T)
passive_t <- aggregate(VAR_passive_t, list(unstrat$person_exposed[unstrat$exposure_subclass=="passive smoke exposure"&unstrat$exposure_type=="binary"],
                                           unstrat$exposure_time[unstrat$exposure_subclass=="passive smoke exposure"&unstrat$exposure_type=="binary"]), FUN=max,na.rm=T)
binge_t <- aggregate(VAR_binge_t, list(unstrat$person_exposed[unstrat$exposure_subclass=="binge drinking"&unstrat$exposure_type=="binary"],
                                       unstrat$exposure_time[unstrat$exposure_subclass=="binge drinking"&unstrat$exposure_type=="binary"]), FUN=max,na.rm=T)
educ_t <- aggregate(VAR_educ_t, list(unstrat$person_exposed[unstrat$exposure_subclass=="low SEP (education)"&unstrat$exposure_type=="binary"],
                                     unstrat$exposure_time[unstrat$exposure_subclass=="low SEP (education)"&unstrat$exposure_type=="binary"]), FUN=max,na.rm=T)
occup_t <- aggregate(VAR_occup_t, list(unstrat$person_exposed[unstrat$exposure_subclass=="low SEP (occupation)"&unstrat$exposure_type=="binary"],
                                       unstrat$exposure_time[unstrat$exposure_subclass=="low SEP (occupation)"&unstrat$exposure_type=="binary"]), FUN=max,na.rm=T)


cohort_table <- data.frame(cohort=c(rep(cohortname,123)),
                     exposure=c(rep("Smoking: active",sum(nrow(as_b),nrow(as_o))),rep("Alcohol: any",sum(nrow(aa_b),nrow(aa_o))), rep("Caffeine: any source",sum(nrow(ac_b),nrow(ac_o))),
                                rep("Smoking: passive",nrow(passive)),rep("Alcohol: binge",nrow(binge)),rep("Low SEP: education",nrow(educ)),rep("Low SEP: occupation",nrow(occup))),
                     parent=c(as_b$Group.1,as_o$Group.1,aa_b$Group.1,aa_o$Group.1,ac_b$Group.1,ac_o$Group.1,
                              passive$Group.1,binge$Group.1,educ$Group.1,occup$Group.1),
                     time=c(as_b$Group.2,as_o$Group.2,aa_b$Group.2,aa_o$Group.2,ac_b$Group.2,ac_o$Group.2,
                            passive$Group.2,binge$Group.2,educ$Group.2,occup$Group.2),
                     dose=c(rep("any",nrow(as_b)),as_o$Group.3,rep("any",nrow(aa_b)),aa_o$Group.3,rep("any",nrow(ac_b)),ac_o$Group.3,
                            rep("any",nrow(passive)),rep("any",nrow(binge)),rep("any",nrow(educ)),rep("any",nrow(occup))),
                     max_exposed=c(as_b$x,as_o$x,aa_b$x,aa_o$x,ac_b$x,ac_o$x,passive$x,binge$x,educ$x,occup$x),
                     max=c(as_b_t$x,as_o_t$x,aa_b_t$x,aa_o_t$x,ac_b_t$x,ac_o_t$x,passive_t$x,binge_t$x,educ_t$x,occup_t$x)
)
cohort_table$percent_exposed <- round(100*(cohort_table$max_exposed/cohort_table$max),0)
cohort_table
}

### ALSPAC
VAR_b_s <- unstrat$exposure_n_ALSPAC[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="binary"]
VAR_o_s <- unstrat$exposure_n_ALSPAC[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="ordinal"]
VAR_to_s <- unstrat$n_ALSPAC[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="ordinal"]
VAR_tb_s <- unstrat$n_ALSPAC[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="binary"]
VAR_o_a <- unstrat$exposure_n_ALSPAC[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="ordinal"]
VAR_b_a <- unstrat$exposure_n_ALSPAC[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="binary"]
VAR_to_a <- unstrat$n_ALSPAC[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="ordinal"]
VAR_tb_a <- unstrat$n_ALSPAC[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="binary"]
VAR_o_c <- unstrat$exposure_n_ALSPAC[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="ordinal"]
VAR_b_c <- unstrat$exposure_n_ALSPAC[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="binary"]
VAR_to_c <- unstrat$n_ALSPAC[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="ordinal"]
VAR_tb_c <- unstrat$n_ALSPAC[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="binary"]
VAR_passive_t <- unstrat$n_ALSPAC[unstrat$exposure_subclass=="passive smoke exposure"&unstrat$exposure_type=="binary"]
VAR_educ_t <- unstrat$n_ALSPAC[unstrat$exposure_subclass=="low SEP (education)"&unstrat$exposure_type=="binary"]
VAR_binge_t <- unstrat$n_ALSPAC[unstrat$exposure_subclass=="binge drinking"&unstrat$exposure_type=="binary"]
VAR_occup_t <- unstrat$n_ALSPAC[unstrat$exposure_subclass=="low SEP (occupation)"&unstrat$exposure_type=="binary"]
VAR_passive <- unstrat$exposure_n_ALSPAC[unstrat$exposure_subclass=="passive smoke exposure"&unstrat$exposure_type=="binary"]
VAR_educ <- unstrat$exposure_n_ALSPAC[unstrat$exposure_subclass=="low SEP (education)"&unstrat$exposure_type=="binary"]
VAR_binge <- unstrat$exposure_n_ALSPAC[unstrat$exposure_subclass=="binge drinking"&unstrat$exposure_type=="binary"]
VAR_occup <- unstrat$exposure_n_ALSPAC[unstrat$exposure_subclass=="low SEP (occupation)"&unstrat$exposure_type=="binary"]

alspac_summary <- make_cohort_summary(VAR_b_s,VAR_o_s,VAR_o_a,VAR_b_a,VAR_o_c,VAR_b_c,VAR_to_s,VAR_to_a,VAR_to_c,VAR_tb_s,VAR_tb_a,VAR_tb_c,VAR_passive,VAR_passive_t, VAR_binge_t,VAR_binge,VAR_educ_t,VAR_educ,VAR_occup_t,VAR_occup,"ALSPAC")

### MOBA
VAR_b_s <- unstrat$exposure_n_MOBA[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="binary"]
VAR_o_s <- unstrat$exposure_n_MOBA[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="ordinal"]
VAR_to_s <- unstrat$n_MOBA[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="ordinal"]
VAR_tb_s <- unstrat$n_MOBA[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="binary"]
VAR_o_a <- unstrat$exposure_n_MOBA[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="ordinal"]
VAR_b_a <- unstrat$exposure_n_MOBA[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="binary"]
VAR_to_a <- unstrat$n_MOBA[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="ordinal"]
VAR_tb_a <- unstrat$n_MOBA[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="binary"]
VAR_o_c <- unstrat$exposure_n_MOBA[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="ordinal"]
VAR_b_c <- unstrat$exposure_n_MOBA[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="binary"]
VAR_to_c <- unstrat$n_MOBA[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="ordinal"]
VAR_tb_c <- unstrat$n_MOBA[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="binary"]
VAR_passive_t <- unstrat$n_MOBA[unstrat$exposure_subclass=="passive smoke exposure"&unstrat$exposure_type=="binary"]
VAR_educ_t <- unstrat$n_MOBA[unstrat$exposure_subclass=="low SEP (education)"&unstrat$exposure_type=="binary"]
VAR_binge_t <- unstrat$n_MOBA[unstrat$exposure_subclass=="binge drinking"&unstrat$exposure_type=="binary"]
VAR_occup_t <- unstrat$n_MOBA[unstrat$exposure_subclass=="low SEP (occupation)"&unstrat$exposure_type=="binary"]
VAR_passive <- unstrat$exposure_n_MOBA[unstrat$exposure_subclass=="passive smoke exposure"&unstrat$exposure_type=="binary"]
VAR_educ <- unstrat$exposure_n_MOBA[unstrat$exposure_subclass=="low SEP (education)"&unstrat$exposure_type=="binary"]
VAR_binge <- unstrat$exposure_n_MOBA[unstrat$exposure_subclass=="binge drinking"&unstrat$exposure_type=="binary"]
VAR_occup <- unstrat$exposure_n_MOBA[unstrat$exposure_subclass=="low SEP (occupation)"&unstrat$exposure_type=="binary"]

moba_summary <- make_cohort_summary(VAR_b_s,VAR_o_s,VAR_o_a,VAR_b_a,VAR_o_c,VAR_b_c,VAR_to_s,VAR_to_a,VAR_to_c,VAR_tb_s,VAR_tb_a,VAR_tb_c,VAR_passive,VAR_passive_t, VAR_binge_t,VAR_binge,VAR_educ_t,VAR_educ,VAR_occup_t,VAR_occup,"MOBA")


### MCS

VAR_b_s <- unstrat$exposure_n_MCS[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="binary"]
VAR_o_s <- unstrat$exposure_n_MCS[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="ordinal"]
VAR_to_s <- unstrat$n_MCS[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="ordinal"]
VAR_tb_s <- unstrat$n_MCS[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="binary"]
VAR_o_a <- unstrat$exposure_n_MCS[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="ordinal"]
VAR_b_a <- unstrat$exposure_n_MCS[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="binary"]
VAR_to_a <- unstrat$n_MCS[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="ordinal"]
VAR_tb_a <- unstrat$n_MCS[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="binary"]
VAR_o_c <- unstrat$exposure_n_MCS[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="ordinal"]
VAR_b_c <- unstrat$exposure_n_MCS[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="binary"]
VAR_to_c <- unstrat$n_MCS[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="ordinal"]
VAR_tb_c <- unstrat$n_MCS[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="binary"]
VAR_passive_t <- unstrat$n_MCS[unstrat$exposure_subclass=="passive smoke exposure"&unstrat$exposure_type=="binary"]
VAR_educ_t <- unstrat$n_MCS[unstrat$exposure_subclass=="low SEP (education)"&unstrat$exposure_type=="binary"]
VAR_binge_t <- unstrat$n_MCS[unstrat$exposure_subclass=="binge drinking"&unstrat$exposure_type=="binary"]
VAR_occup_t <- unstrat$n_MCS[unstrat$exposure_subclass=="low SEP (occupation)"&unstrat$exposure_type=="binary"]
VAR_passive <- unstrat$exposure_n_MCS[unstrat$exposure_subclass=="passive smoke exposure"&unstrat$exposure_type=="binary"]
VAR_educ <- unstrat$exposure_n_MCS[unstrat$exposure_subclass=="low SEP (education)"&unstrat$exposure_type=="binary"]
VAR_binge <- unstrat$exposure_n_MCS[unstrat$exposure_subclass=="binge drinking"&unstrat$exposure_type=="binary"]
VAR_occup <- unstrat$exposure_n_MCS[unstrat$exposure_subclass=="low SEP (occupation)"&unstrat$exposure_type=="binary"]

mcs_summary <- make_cohort_summary(VAR_b_s,VAR_o_s,VAR_o_a,VAR_b_a,VAR_o_c,VAR_b_c,VAR_to_s,VAR_to_a,VAR_to_c,VAR_tb_s,VAR_tb_a,VAR_tb_c,VAR_passive,VAR_passive_t, VAR_binge_t,VAR_binge,VAR_educ_t,VAR_educ,VAR_occup_t,VAR_occup,"MCS")


### BIB

VAR_b_s <- unstrat$exposure_n_BIB[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="binary"]
VAR_o_s <- unstrat$exposure_n_BIB[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="ordinal"]
VAR_to_s <- unstrat$n_BIB[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="ordinal"]
VAR_tb_s <- unstrat$n_BIB[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="binary"]
VAR_o_a <- unstrat$exposure_n_BIB[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="ordinal"]
VAR_b_a <- unstrat$exposure_n_BIB[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="binary"]
VAR_to_a <- unstrat$n_BIB[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="ordinal"]
VAR_tb_a <- unstrat$n_BIB[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="binary"]
VAR_o_c <- unstrat$exposure_n_BIB[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="ordinal"]
VAR_b_c <- unstrat$exposure_n_BIB[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="binary"]
VAR_to_c <- unstrat$n_BIB[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="ordinal"]
VAR_tb_c <- unstrat$n_BIB[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="binary"]
VAR_passive_t <- unstrat$n_BIB[unstrat$exposure_subclass=="passive smoke exposure"&unstrat$exposure_type=="binary"]
VAR_educ_t <- unstrat$n_BIB[unstrat$exposure_subclass=="low SEP (education)"&unstrat$exposure_type=="binary"]
VAR_binge_t <- unstrat$n_BIB[unstrat$exposure_subclass=="binge drinking"&unstrat$exposure_type=="binary"]
VAR_occup_t <- unstrat$n_BIB[unstrat$exposure_subclass=="low SEP (occupation)"&unstrat$exposure_type=="binary"]
VAR_passive <- unstrat$exposure_n_BIB[unstrat$exposure_subclass=="passive smoke exposure"&unstrat$exposure_type=="binary"]
VAR_educ <- unstrat$exposure_n_BIB[unstrat$exposure_subclass=="low SEP (education)"&unstrat$exposure_type=="binary"]
VAR_binge <- unstrat$exposure_n_BIB[unstrat$exposure_subclass=="binge drinking"&unstrat$exposure_type=="binary"]
VAR_occup <- unstrat$exposure_n_BIB[unstrat$exposure_subclass=="low SEP (occupation)"&unstrat$exposure_type=="binary"]

bib_summary <- make_cohort_summary(VAR_b_s,VAR_o_s,VAR_o_a,VAR_b_a,VAR_o_c,VAR_b_c,VAR_to_s,VAR_to_a,VAR_to_c,VAR_tb_s,VAR_tb_a,VAR_tb_c,VAR_passive,VAR_passive_t, VAR_binge_t,VAR_binge,VAR_educ_t,VAR_educ,VAR_occup_t,VAR_occup,"BIB")

### TOTAL

VAR_b_s <- unstrat$total_n_exposure[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="binary"]
VAR_o_s <- unstrat$total_n_exposure[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="ordinal"]
VAR_to_s <- unstrat$total_n[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="ordinal"]
VAR_tb_s <- unstrat$total_n[unstrat$exposure_subclass=="active smoking"&unstrat$exposure_type=="binary"]
VAR_o_a <- unstrat$total_n_exposure[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="ordinal"]
VAR_b_a <- unstrat$total_n_exposure[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="binary"]
VAR_to_a <- unstrat$total_n[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="ordinal"]
VAR_tb_a <- unstrat$total_n[unstrat$exposure_subclass=="any drinking"&unstrat$exposure_type=="binary"]
VAR_o_c <- unstrat$total_n_exposure[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="ordinal"]
VAR_b_c <- unstrat$total_n_exposure[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="binary"]
VAR_to_c <- unstrat$total_n[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="ordinal"]
VAR_tb_c <- unstrat$total_n[unstrat$exposure_subclass=="any source"&unstrat$exposure_type=="binary"]
VAR_passive_t <- unstrat$total_n[unstrat$exposure_subclass=="passive smoke exposure"&unstrat$exposure_type=="binary"]
VAR_educ_t <- unstrat$total_n[unstrat$exposure_subclass=="low SEP (education)"&unstrat$exposure_type=="binary"]
VAR_binge_t <- unstrat$total_n[unstrat$exposure_subclass=="binge drinking"&unstrat$exposure_type=="binary"]
VAR_occup_t <- unstrat$total_n[unstrat$exposure_subclass=="low SEP (occupation)"&unstrat$exposure_type=="binary"]
VAR_passive <- unstrat$total_n_exposure[unstrat$exposure_subclass=="passive smoke exposure"&unstrat$exposure_type=="binary"]
VAR_educ <- unstrat$total_n_exposure[unstrat$exposure_subclass=="low SEP (education)"&unstrat$exposure_type=="binary"]
VAR_binge <- unstrat$total_n_exposure[unstrat$exposure_subclass=="binge drinking"&unstrat$exposure_type=="binary"]
VAR_occup <- unstrat$total_n_exposure[unstrat$exposure_subclass=="low SEP (occupation)"&unstrat$exposure_type=="binary"]

total_summary <- make_cohort_summary(VAR_b_s,VAR_o_s,VAR_o_a,VAR_b_a,VAR_o_c,VAR_b_c,VAR_to_s,VAR_to_a,VAR_to_c,VAR_tb_s,VAR_tb_a,VAR_tb_c,VAR_passive,VAR_passive_t, VAR_binge_t,VAR_binge,VAR_educ_t,VAR_educ,VAR_occup_t,VAR_occup,"Total")

## combine
all_cohort_summaries <- rbind(alspac_summary,bib_summary,mcs_summary,moba_summary,total_summary)
all_cohort_summaries$exposure_dose <- paste0(all_cohort_summaries$exposure," (",all_cohort_summaries$dose,")")
all_cohort_summaries$value <- paste0(all_cohort_summaries$max_exposed,"/",all_cohort_summaries$max,
       " (",all_cohort_summaries$percent_exposed,"%)")
all_cohort_summaries$parent_time <- paste0(all_cohort_summaries$parent,": ",all_cohort_summaries$time)

WIDE <- all_cohort_summaries[,c("cohort","exposure_dose","parent","time","value")] %>%
pivot_wider(names_from = c(parent), values_from = c(value))

write.csv(all_cohort_summaries,file="cohort_summaries_n_long.csv")
write.csv(WIDE,file="cohort_summaries_n.csv")

#comparing maternal and paternal effect sizes for model 2a and 2b, separately for HBs and SEP
HBs <- c("smoking","alcohol consumption","caffeine consumption")
mat <- unstrat[unstrat$person_exposed=="mother",]
pat <- unstrat[unstrat$person_exposed=="partner",]
matpat <- merge(mat,pat,by=c("exposure_class","exposure_subclass","exposure_time","exposure_type","exposure_dose","outcome_linker","model"),all=T,suffixes = c("mat","pat"))
matpat$samedir <- sign(matpat$estmat)==sign(matpat$estpat)
prop.table(table(matpat$samedir[matpat$model=="model2a"&matpat$exposure_class%in%HBs])) #64% in same direction
prop.table(table(matpat$samedir[matpat$model=="model2b"&matpat$exposure_class%in%HBs])) #59% in same direction
prop.table(table(matpat$samedir[matpat$model=="model2a"&matpat$exposure_class=="low socioeconomic position"])) #83% in same direction
prop.table(table(matpat$samedir[matpat$model=="model2b"&matpat$exposure_class=="low socioeconomic position"])) #80% in same direction
matpat$samedir_diff <- abs(matpat$estmat) - abs(matpat$estpat)
matpat$samedir_diff[matpat$samedir==F]<-NA
prop.table(table(sign(matpat$samedir_diff[matpat$model=="model2a"&matpat$exposure_class%in%HBs]))) #48% mat>pat, 52% pat>mat
prop.table(table(sign(matpat$samedir_diff[matpat$model=="model2b"&matpat$exposure_class%in%HBs]))) #54% mat>pat, 46% pat>mat
prop.table(table(sign(matpat$samedir_diff[matpat$model=="model2a"&matpat$exposure_class=="low socioeconomic position"]))) #66% mat>pat, 34% pat>mat
prop.table(table(sign(matpat$samedir_diff[matpat$model=="model2b"&matpat$exposure_class=="low socioeconomic position"]))) #72% mat>pat, 28% pat>mat

matpat$samedir_pc_change <- (abs(matpat$samedir_diff)/abs(matpat$estmat))*100
summary(matpat$samedir_pc_change[matpat$samedir_diff<0&matpat$model=="model2a"&matpat$exposure_class%in%HBs])#P = median 124% > than M
summary(matpat$samedir_pc_change[matpat$samedir_diff>0&matpat$model=="model2a"&matpat$exposure_class%in%HBs])#P = median 51% < than M
summary(matpat$samedir_pc_change[matpat$samedir_diff<0&matpat$model=="model2b"&matpat$exposure_class%in%HBs])#P = median 143% > than M
summary(matpat$samedir_pc_change[matpat$samedir_diff>0&matpat$model=="model2b"&matpat$exposure_class%in%HBs])#P = median 57% < than M

summary(matpat$samedir_pc_change[matpat$samedir_diff<0&matpat$model=="model2a"&matpat$exposure_class=="low socioeconomic position"])#P = median 45% > than M
summary(matpat$samedir_pc_change[matpat$samedir_diff>0&matpat$model=="model2a"&matpat$exposure_class=="low socioeconomic position"])#P = median 29% < than M
summary(matpat$samedir_pc_change[matpat$samedir_diff<0&matpat$model=="model2b"&matpat$exposure_class=="low socioeconomic position"])#P = median 59% > than M
summary(matpat$samedir_pc_change[matpat$samedir_diff>0&matpat$model=="model2b"&matpat$exposure_class=="low socioeconomic position"])#P = median 43% < than M

##continuous
ggmatpat <- matpat[matpat$model=="model2b"&matpat$outcome_typemat=="continuous",]
ggmatpat <- ggmatpat[order(ggmatpat$estmat),]
ggmatpat$exposure_class <- factor(ggmatpat$exposure_class,ordered=T,levels=c("smoking","alcohol consumption","caffeine consumption","low socioeconomic position"))
require(ggplot2)
ggplot(ggmatpat,aes(y=1:nrow(ggmatpat)))+
  geom_point(aes(x=estpat),colour="cornflowerblue",alpha=0.5)+
  geom_vline(xintercept=0)+
  geom_point(aes(x=estmat),colour="darkred",alpha=0.5)+
  theme_classic()+
  facet_wrap(exposure_class~.)+
  theme(axis.text.y=element_blank())+ylab("Outcomes\n(ordered by maternal effect estimate)")+
  xlab("Effect estimate\n(standardised mean difference)")

ggmatpat_ridges <- data.frame(est=c(ggmatpat$estmat,ggmatpat$estpat),
                              parent=c(rep("mother",nrow(ggmatpat)),rep("partner",nrow(ggmatpat))),
                              exposure_class=c(ggmatpat$exposure_class,ggmatpat$exposure_class))
require(ggridges)
ggplot(ggmatpat_ridges)+
  geom_density_ridges(aes(x=est,y=parent,fill=parent),alpha=0.5,scale=1)+
  facet_wrap(exposure_class~.)+
  scale_fill_manual(values = c("darkred", "cornflowerblue"))+
  theme_classic()+ylab("")+xlab("Effect estimate\n(standardised mean difference)")+
  theme(legend.position = "null")+
  scale_y_discrete(expand=c(0.1,0))

## binary
ggmatpat <- matpat[matpat$model=="model2b"&matpat$outcome_typemat=="binary",]
ggmatpat <- ggmatpat[order(ggmatpat$estmat),]
ggmatpat$exposure_class <- factor(ggmatpat$exposure_class,ordered=T,levels=c("smoking","alcohol consumption","caffeine consumption","low socioeconomic position"))
require(ggplot2)
ggplot(ggmatpat,aes(y=1:nrow(ggmatpat)))+
  geom_point(aes(x=estpat),colour="cornflowerblue",alpha=0.5)+
  geom_vline(xintercept=1)+
  geom_point(aes(x=estmat),colour="darkred",alpha=0.5)+
  theme_classic()+
  facet_wrap(exposure_class~.)+
  theme(axis.text.y=element_blank())+ylab("Outcomes\n(ordered by maternal effect estimate)")+
  xlab("Effect estimate\n(log odds ratio)")

ggmatpat_ridges <- data.frame(est=c(ggmatpat$estmat,ggmatpat$estpat),
                              parent=c(rep("mother",nrow(ggmatpat)),rep("partner",nrow(ggmatpat))),
                              exposure_class=c(ggmatpat$exposure_class,ggmatpat$exposure_class))
ggplot(ggmatpat_ridges)+
  geom_density_ridges(aes(x=est,y=parent,fill=parent),alpha=0.5,scale=1)+
  facet_wrap(exposure_class~.)+
  scale_fill_manual(values = c("darkred", "cornflowerblue"))+
  theme_classic()+ylab("")+xlab("Effect estimate\n(log odds ratio)")+
  theme(legend.position = "null")+
  scale_y_discrete(expand=c(0.1,0))

#small effect sizes
summary(abs(matpat$estmat[matpat$model=="model2b"&matpat$outcome_typemat=="continuous"&matpat$exposure_class%in%HBs]))#min 1e-07., max 1.23, median 0.06
summary(abs(matpat$estpat[matpat$model=="model2b"&matpat$outcome_typemat=="continuous"&matpat$exposure_class%in%HBs]))#min 1e-07., max 1.45, median 0.06
summary(exp(abs(matpat$estmat[matpat$model=="model2b"&matpat$outcome_typemat=="binary"&matpat$exposure_class%in%HBs])))#min 1, max 17.9, median 1.16
summary(exp(abs(matpat$estpat[matpat$model=="model2b"&matpat$outcome_typemat=="binary"&matpat$exposure_class%in%HBs])))#min 1, max 10.6, median 1.16

summary(abs(matpat$estmat[matpat$model=="model2b"&matpat$outcome_typemat=="continuous"&matpat$exposure_class=="low socioeconomic position"]))#min 0.002, max 0.66, median 0.10
summary(abs(matpat$estpat[matpat$model=="model2b"&matpat$outcome_typemat=="continuous"&matpat$exposure_class=="low socioeconomic position"]))#min 0.0003., max 0.5, median 0.09
summary(exp(abs(matpat$estmat[matpat$model=="model2b"&matpat$outcome_typemat=="binary"&matpat$exposure_class=="low socioeconomic position"])))#min 1.001, max 4.6, median 1.26
summary(exp(abs(matpat$estpat[matpat$model=="model2b"&matpat$outcome_typemat=="binary"&matpat$exposure_class=="low socioeconomic position"])))#min 1.002., max 4.8, median 1.17


#Large P
prop.table(table(matpat$fdrmat[matpat$model=="model2b"&matpat$exposure_class%in%HBs]<0.05)) #17%
prop.table(table(matpat$fdrpat[matpat$model=="model2b"&matpat$exposure_class%in%HBs]<0.05)) #12%
n_hits <- aggregate(dat$fdr, list(paste(dat$model_new_name,dat$person_exposed,dat$exposure_class)), FUN=function(x){sum(x<0.05)})
names(n_hits) <- c("model","hits")
tmp <- aggregate(dat$fdr, list(paste(dat$model_new_name,dat$person_exposed,dat$exposure_class)), FUN=function(x){sum(x<1)})
n_hits$total <-tmp$x
n_hits$pc <- (n_hits$hits/n_hits$total)*100
n_hits$mutual_adjustment <-"Not adjusted for co-parent"
n_hits$mutual_adjustment[grep("with co-parent",n_hits$model)]<-"Adjusted for co-parent"
n_hits$adj_set <-"Minimal"
n_hits$adj_set[grep("Standard",n_hits$model)]<-"Standard"
n_hits$timing <-NA
n_hits$timing[grep("timepoint",n_hits$model)]<-"Plus previous timepoint(s)"
n_hits$mediators <-NA
n_hits$mediators[grep("mediators|child's GRS",n_hits$model)]<-"Plus potential mediators"
n_hits$grs <-"Observational"
n_hits$grs[grep("GRS",n_hits$model)]<-"GRS"
n_hits$person <-"mother"
n_hits$person[grep("partner",n_hits$model)]<-"partner"
n_hits$exposure<-"smoking"
n_hits$exposure[grep("alcohol",n_hits$model)]<-"alcohol consumption"
n_hits$exposure[grep("caffeine",n_hits$model)]<-"caffeine consumption"
n_hits$exposure[grep("socio",n_hits$model)]<-"low socioeconomic position"

ggplot(n_hits[-grep(n_hits$model,pattern="stratified"),],aes(x=pc,y=paste(grs,adj_set,timing,mediators),fill=mutual_adjustment))+
         geom_col(position=position_dodge(0.8),width=0.7)+
  geom_text(aes(x=pc+4,label=hits),size=3,position=position_dodge(0.8))+
  facet_grid(person~exposure)

#qq plot
ggplot(ggmatpat)+
  stat_qq(aes(sample=-log10(pmat)),colour="darkred") + stat_qq_line(aes(sample=-log10(pmat)),colour="darkred")+
  stat_qq(aes(sample=-log10(ppat)),colour="cornflowerblue") + stat_qq_line(aes(sample=-log10(ppat)),colour="cornflowerblue")+
  theme_classic()

#Model 2B upturned manhattan overview
mt_m <- unstrat[unstrat$model=="model2b",c("outcome_type","person_exposed","exposure_class","exposure_subclass","exposure_time","exposure_dose","exposure_type","outcome_class","outcome_subclass2","outcome_time","est","fdr","outcome_subclass1","p")]
mt_m$exposure <- paste(mt_m$exposure_class,mt_m$exposure_subclass,mt_m$exposure_time,mt_m$exposure_dose,mt_m$exposure_type)
mt_m$outcome <- paste(mt_m$outcome_class,mt_m$outcome_subclass2,mt_m$outcome_time)
mt_m$fdr_alpha <- 0.2
mt_m$fdr_alpha[mt_m$fdr<0.05] <- 0.5
mt_m$fdr_alpha[mt_m$fdr<0.005] <- 0.6
mt_m$fdr_alpha[mt_m$fdr<0.0005] <- 0.7
mt_m$fdr_alpha[mt_m$fdr<0.00005] <- 0.8
mt_m$fdr_alpha[mt_m$fdr<0.000005] <- 0.9
mt_m$fdr_alpha[mt_m$fdr<0.0000005] <- 1.0
# to convert ln(OR) to cohen's d (SDM), multiply ln(OR) by sqrt(3)/pi (which is 0.5513)
mt_m$est_SDM <- mt_m$est
mt_m$est_SDM[mt_m$outcome_type=="binary"]<-mt_m$est[mt_m$outcome_type=="binary"]*0.5513

mt_m$exposure_class <- factor(mt_m$exposure_class, ordered=T, levels=c("smoking","alcohol consumption", "caffeine consumption","low socioeconomic position"))
require(RColorBrewer)
ggplot(mt_m,aes(y=outcome_subclass2,x=est_SDM,colour=exposure_class))+
  geom_jitter(aes(alpha=fdr_alpha*0.7))+
  geom_vline(xintercept=0)+
  scale_colour_manual(values=brewer.pal(8, "Dark2")[2:8])+
  facet_grid(outcome_class~person_exposed,space = "free_y",scales="free_y")+
  theme_minimal()+xlab("Standardised effect estimate (Cohen's D)")+ylab("Child outcomes")+
  theme(strip.text.y=element_blank())

ggplot(mt_m,aes(y=outcome_subclass2,x=est_SDM,colour=fdr<0.05))+
  geom_jitter(aes(alpha=fdr_alpha*0.7))+
  geom_vline(xintercept=0)+
  scale_colour_manual(values=brewer.pal(8, "Dark2")[c(8,4)])+
  facet_grid(outcome_class~exposure_class+person_exposed,space = "free_y",scales="free_y")+
  theme_classic()+xlab("Standardised effect estimate (Cohen's D)")+ylab("Child outcomes")+
  theme(strip.text.y=element_blank())

# strongest associations
hits_bin <- unstrat[unstrat$fdr<0.05&unstrat$cohorts_n>1&unstrat$outcome_type=="binary",]
hits_cont <- unstrat[unstrat$fdr<0.05&unstrat$cohorts_n>1&unstrat$outcome_type=="continuous",]
require(tidyverse)

mean_estimates <- hits_bin %>%
  group_by(paste(person_exposed,exposure_subclass,outcome_subclass2)) %>%
  summarise_at(vars(est), list(mean_est = mean, max_est=max, min_est=min))
mean_estimates[order(mean_estimates$mean_est),]->mean_estimates_bin
mean_estimates_bin$mean_or <- exp(mean_estimates_bin$mean_est)
mean_estimates_bin$min_or <- exp(mean_estimates_bin$min_est)
mean_estimates_bin$max_or <- exp(mean_estimates_bin$max_est)

mean_estimates <- hits_cont %>%
  group_by(paste(person_exposed,exposure_subclass,outcome_subclass2)) %>%
  summarise_at(vars(est), list(mean_est = mean, max_est=max, min_est=min))
mean_estimates[order(mean_estimates$mean_est),]->mean_estimates_cont


# triangulation with heatmap MOTHERS

## MVR
top_hits_2a <- unstrat[unstrat$exposure_subclass!="genetic risk score"&unstrat$fdr<0.05&unstrat$model=="model2a",c("person_exposed","exposure_class","exposure_subclass","exposure_time","exposure_type","exposure_dose","outcome_class","outcome_subclass1","outcome_subclass2","outcome_time","outcome_type","est","se","p","i2","hetp","cohorts","cohorts_n","total_n")] #model 2a for MVR,obs unstrata only
top_hits_2a$combination <- apply(top_hits_2a[,c("outcome_class","outcome_subclass1","outcome_subclass2","outcome_time","outcome_type")],
                                 1,paste,collapse="_")
top_hits_2a_m <- top_hits_2a[top_hits_2a$person_exposed=="mother"&top_hits_2a$exposure_time=="ever in pregnancy",]
tdat_s <- data.frame(outcome = top_hits_2a_m$combination[top_hits_2a_m$exposure_class=="smoking"],
                   sign=sign(top_hits_2a_m$est[top_hits_2a_m$exposure_class=="smoking"]))
tdat_a <- data.frame(outcome = top_hits_2a_m$combination[top_hits_2a_m$exposure_class=="alcohol consumption"],
                     sign=sign(top_hits_2a_m$est[top_hits_2a_m$exposure_class=="alcohol consumption"]))
tdat_c <- data.frame(outcome = top_hits_2a_m$combination[top_hits_2a_m$exposure_class=="caffeine consumption"],
                     sign=sign(top_hits_2a_m$est[top_hits_2a_m$exposure_class=="caffeine consumption"]))

## GRS
top_hits_grs <- unstrat[unstrat$exposure_subclass=="genetic risk score"&unstrat$p<0.05&unstrat$model=="model2a",] #model 2a for grs
top_hits_grs$combination <- apply(top_hits_grs[,c("outcome_class","outcome_subclass1","outcome_subclass2","outcome_time","outcome_type")],
                                  1,paste,collapse="_")
top_hits_grs_m <- top_hits_grs[top_hits_grs$person_exposed=="mother",]
tdat_s_grs <- data.frame(outcome = top_hits_grs_m$combination[top_hits_grs_m$exposure_class=="smoking"&top_hits_grs_m$exposure_time=="initiation"],
                     sign=sign(top_hits_grs_m$est[top_hits_grs_m$exposure_class=="smoking"&top_hits_grs_m$exposure_time=="initiation"]))
tdat_a_grs <- data.frame(outcome = top_hits_grs_m$combination[top_hits_grs_m$exposure_class=="alcohol consumption"],
                     sign=sign(top_hits_grs_m$est[top_hits_grs_m$exposure_class=="alcohol consumption"]))
tdat_c_grs <- data.frame(outcome = top_hits_grs_m$combination[top_hits_grs_m$exposure_class=="caffeine consumption"],
                     sign=sign(top_hits_grs_m$est[top_hits_grs_m$exposure_class=="caffeine consumption"]))

tdat_s <- merge(tdat_s,tdat_s_grs,by="outcome",all.x=T,all.y=F,suffixes=c("mvr","grs"))
tdat_s <-tdat_s[duplicated(tdat_s$outcome)==F,]
tdat_a <- merge(tdat_a,tdat_a_grs,by="outcome",all.x=T,all.y=F,suffixes=c("mvr","grs"))
tdat_a <-tdat_a[duplicated(tdat_a$outcome)==F,]
tdat_c <- merge(tdat_c,tdat_c_grs,by="outcome",all.x=T,all.y=F,suffixes=c("mvr","grs"))
tdat_c <-tdat_c[duplicated(tdat_c$outcome)==F,]

# Neg Con
top_hits_negcon_m <- matpat[matpat$exposure_subclass!="genetic risk score"&matpat$pmat<0.05 & matpat$samedir==T & matpat$samedir_diff>0& matpat$model=="model2b",] #mutually adjusted model, mat>pat and in same dir, and matpat p<0.05
top_hits_negcon_m$combination <- apply(top_hits_negcon_m[,c("outcome_classmat","outcome_subclass1mat","outcome_subclass2mat","outcome_timemat","outcome_typemat")],
                                  1,paste,collapse="_")
top_hits_negcon_m <- top_hits_negcon_m[top_hits_negcon_m$exposure_time=="ever in pregnancy",]

tdat_s_negcon <- data.frame(outcome = top_hits_negcon_m$combination[top_hits_negcon_m$exposure_class=="smoking"],
                         signnegcon=sign(top_hits_negcon_m$estmat[top_hits_negcon_m$exposure_class=="smoking"]))
tdat_a_negcon <- data.frame(outcome = top_hits_negcon_m$combination[top_hits_negcon_m$exposure_class=="alcohol consumption"],
                            signnegcon=sign(top_hits_negcon_m$estmat[top_hits_negcon_m$exposure_class=="alcohol consumption"]))
tdat_c_negcon <- data.frame(outcome = top_hits_negcon_m$combination[top_hits_negcon_m$exposure_class=="caffeine consumption"],
                            signnegcon=sign(top_hits_negcon_m$estmat[top_hits_negcon_m$exposure_class=="caffeine consumption"]))

tdat_s <- merge(tdat_s,tdat_s_negcon,by="outcome",all=T)
tdat_s <-tdat_s[duplicated(tdat_s$outcome)==F,]
tdat_a <- merge(tdat_a,tdat_a_negcon,by="outcome",all=T)
tdat_a <-tdat_a[duplicated(tdat_a$outcome)==F,]
tdat_c <- merge(tdat_c,tdat_c_negcon,by="outcome",all=T)
tdat_c <-tdat_c[duplicated(tdat_c$outcome)==F,]

tdat_s[tdat_s==(-1)]<-"negative\nassociation"
tdat_s[tdat_s==(1)]<-"positive\nassociation"
tdat_c[tdat_c==(-1)]<-"negative\nassociation"
tdat_c[tdat_c==(1)]<-"positive\nassociation"
tdat_a[tdat_a==(-1)]<-"negative\nassociation"
tdat_a[tdat_a==(1)]<-"positive\nassociation"

## plot

## heatmap

tdat_s$total <- "1 line\nof evidence"
tdat_s$total[tdat_s$signmvr==tdat_s$signgrs]<-"2 lines\nof evidence"
tdat_s$total[tdat_s$signmvr==tdat_s$signnegcon]<-"2 lines\nof evidence"
tdat_s$total[tdat_s$signgrs==tdat_s$signnegcon]<-"2 lines\nof evidence"
tdat_s$total[(tdat_s$signgrs==tdat_s$signnegcon)&(tdat_s$signmvr==tdat_s$signnegcon)]<-"3 lines\nof evidence"

tdat_a$total <- "1 line\nof evidence"
tdat_a$total[tdat_a$signmvr==tdat_a$signgrs]<-"2 lines\nof evidence"
tdat_a$total[tdat_a$signmvr==tdat_a$signnegcon]<-"2 lines\nof evidence"
tdat_a$total[tdat_a$signgrs==tdat_a$signnegcon]<-"2 lines\nof evidence"
tdat_a$total[(tdat_a$signgrs==tdat_a$signnegcon)&(tdat_a$signmvr==tdat_a$signnegcon)]<-"3 lines\nof evidence"

tdat_c$total <- "1 line\nof evidence"
tdat_c$total[tdat_c$signmvr==tdat_c$signgrs]<-"2 lines\nof evidence"
tdat_c$total[tdat_c$signmvr==tdat_c$signnegcon]<-"2 lines\nof evidence"
tdat_c$total[tdat_c$signgrs==tdat_c$signnegcon]<-"2 lines\nof evidence"
tdat_c$total[(tdat_c$signgrs==tdat_c$signnegcon)&(tdat_c$signmvr==tdat_c$signnegcon)]<-"3 lines\nof evidence"

tdat_s_melt <- reshape2::melt(tdat_s,na.rm = F,id.vars="outcome")
tdat_s_melt$exposure<-"smoking"
tdat_s_melt$parent<-"mother"
tdat_c_melt <- reshape2::melt(tdat_c,na.rm = F,id.vars="outcome")
tdat_c_melt$exposure<-"caffeine consumption"
tdat_c_melt$parent<-"mother"
tdat_a_melt <- reshape2::melt(tdat_a,na.rm = F,id.vars="outcome")
tdat_a_melt$exposure<-"alcohol consumption"
tdat_a_melt$parent<-"mother"
tdat_mothers <- rbind(tdat_s_melt,tdat_c_melt,tdat_a_melt)
tdat_mothers <- tdat_mothers[is.na(tdat_mothers$outcome)==F,]

tdat_mothers$outcome_class <- unlist(lapply(lapply(lapply(tdat_mothers$outcome,stringr::str_split,pattern = "_"),unlist),"[",1))
tdat_mothers$outcome_subclass <- paste(unlist(lapply(lapply(lapply(tdat_mothers$outcome,stringr::str_split,pattern = "_"),unlist),"[",3)),
                                        unlist(lapply(lapply(lapply(tdat_mothers$outcome,stringr::str_split,pattern = "_"),unlist),"[",4)),
                                       sep=" at ") 

tdat_mothers$value <- factor(tdat_mothers$value,ordered=T,levels=c("3 lines\nof evidence","2 lines\nof evidence","1 line\nof evidence","negative\nassociation","positive\nassociation"))
tdat_mothers$exposure <- factor(tdat_mothers$exposure,ordered=T,levels=c("smoking","alcohol consumption","caffeine consumption"))
tdat_mothers<-tdat_mothers[order(tdat_mothers$outcome_class,tdat_mothers$outcome_subclass),]
tdat_mothers$outcome_subclass <- factor(tdat_mothers$outcome_subclass,ordered=T,levels=unique(tdat_mothers$outcome_subclass))

tdat_mothers$variable2<-NA
tdat_mothers$variable2[tdat_mothers$variable=="signmvr"]<-"MVR"
tdat_mothers$variable2[tdat_mothers$variable=="signgrs"]<-"GRS"
tdat_mothers$variable2[tdat_mothers$variable=="signnegcon"]<-"NC"
tdat_mothers$variable2[tdat_mothers$variable=="total"]<-"T"


cols <- c("negative\nassociation" = "#fc8d59", "positive\nassociation" = "#91bfdb", "1 line\nof evidence" = "#e5f5e0", "2 lines\nof evidence" = "#a1d99b", "3 lines\nof evidence"="#31a354")

library(ggplot2)
ggplot(tdat_mothers,aes(x=variable2,y=outcome_subclass))+
  geom_tile(aes(fill=factor(value)))+
  scale_fill_manual(values = cols,na.translate=F)+
  facet_grid(.~exposure,scales = "free")+
  theme_classic()+
  theme(axis.text.y=element_text(size=6))








# triangulation with heatmap PARTNERS

## MVR
top_hits_2a <- unstrat[unstrat$exposure_subclass!="genetic risk score"&unstrat$fdr<0.05&unstrat$model=="model2a",c("person_exposed","exposure_class","exposure_subclass","exposure_time","exposure_type","exposure_dose","outcome_class","outcome_subclass1","outcome_subclass2","outcome_time","outcome_type","est","se","p","i2","hetp","cohorts","cohorts_n","total_n")] #model 2a for MVR,obs unstrata only
top_hits_2a$combination <- apply(top_hits_2a[,c("outcome_class","outcome_subclass1","outcome_subclass2","outcome_time","outcome_type")],
                                 1,paste,collapse="_")
top_hits_2a_p <- top_hits_2a[top_hits_2a$person_exposed=="partner"&top_hits_2a$exposure_time=="ever in pregnancy",]
tdat_s <- data.frame(outcome = top_hits_2a_p$combination[top_hits_2a_p$exposure_class=="smoking"],
                     sign=sign(top_hits_2a_p$est[top_hits_2a_p$exposure_class=="smoking"]))
tdat_a <- data.frame(outcome = top_hits_2a_p$combination[top_hits_2a_p$exposure_class=="alcohol consumption"],
                     sign=sign(top_hits_2a_p$est[top_hits_2a_p$exposure_class=="alcohol consumption"]))
tdat_c <- data.frame(outcome = top_hits_2a_p$combination[top_hits_2a_p$exposure_class=="caffeine consumption"],
                     sign=sign(top_hits_2a_p$est[top_hits_2a_p$exposure_class=="caffeine consumption"]))

## GRS
top_hits_grs <- unstrat[unstrat$exposure_subclass=="genetic risk score"&unstrat$p<0.05&unstrat$model=="model2a",] #model 2a for grs
top_hits_grs$combination <- apply(top_hits_grs[,c("outcome_class","outcome_subclass1","outcome_subclass2","outcome_time","outcome_type")],
                                  1,paste,collapse="_")
top_hits_grs_p <- top_hits_grs[top_hits_grs$person_exposed=="partner",]
tdat_s_grs <- data.frame(outcome = top_hits_grs_p$combination[top_hits_grs_p$exposure_class=="smoking"&top_hits_grs_p$exposure_time=="initiation"],
                         sign=sign(top_hits_grs_p$est[top_hits_grs_p$exposure_class=="smoking"&top_hits_grs_p$exposure_time=="initiation"]))
tdat_a_grs <- data.frame(outcome = top_hits_grs_p$combination[top_hits_grs_p$exposure_class=="alcohol consumption"],
                         sign=sign(top_hits_grs_p$est[top_hits_grs_p$exposure_class=="alcohol consumption"]))
tdat_c_grs <- data.frame(outcome = top_hits_grs_p$combination[top_hits_grs_p$exposure_class=="caffeine consumption"],
                         sign=sign(top_hits_grs_p$est[top_hits_grs_p$exposure_class=="caffeine consumption"]))

tdat_s <- merge(tdat_s,tdat_s_grs,by="outcome",all.x=T,all.y=F,suffixes=c("mvr","grs"))
tdat_s <-tdat_s[duplicated(tdat_s$outcome)==F,]
tdat_a <- merge(tdat_a,tdat_a_grs,by="outcome",all.x=T,all.y=F,suffixes=c("mvr","grs"))
tdat_a <-tdat_a[duplicated(tdat_a$outcome)==F,]
tdat_c <- merge(tdat_c,tdat_c_grs,by="outcome",all.x=T,all.y=F,suffixes=c("mvr","grs"))
tdat_c <-tdat_c[duplicated(tdat_c$outcome)==F,]

# Neg Con
top_hits_negcon_p <- matpat[matpat$exposure_subclass!="genetic risk score"&matpat$ppat<0.05 & matpat$samedir==T & matpat$samedir_diff>0& matpat$model=="model2b",] #mutually adjusted model, mat>pat and in same dir, and matpat p<0.05
top_hits_negcon_p$combination <- apply(top_hits_negcon_p[,c("outcome_classmat","outcome_subclass1mat","outcome_subclass2mat","outcome_timemat","outcome_typemat")],
                                       1,paste,collapse="_")
top_hits_negcon_p <- top_hits_negcon_p[top_hits_negcon_p$exposure_time=="ever in pregnancy",]

tdat_s_negcon <- data.frame(outcome = top_hits_negcon_p$combination[top_hits_negcon_p$exposure_class=="smoking"],
                            signnegcon=sign(top_hits_negcon_p$estmat[top_hits_negcon_p$exposure_class=="smoking"]))
tdat_a_negcon <- data.frame(outcome = top_hits_negcon_p$combination[top_hits_negcon_p$exposure_class=="alcohol consumption"],
                            signnegcon=sign(top_hits_negcon_p$estmat[top_hits_negcon_p$exposure_class=="alcohol consumption"]))
tdat_c_negcon <- data.frame(outcome = top_hits_negcon_p$combination[top_hits_negcon_p$exposure_class=="caffeine consumption"],
                            signnegcon=sign(top_hits_negcon_p$estmat[top_hits_negcon_p$exposure_class=="caffeine consumption"]))

tdat_s <- merge(tdat_s,tdat_s_negcon,by="outcome",all=T)
tdat_s <-tdat_s[duplicated(tdat_s$outcome)==F,]
tdat_a <- merge(tdat_a,tdat_a_negcon,by="outcome",all=T)
tdat_a <-tdat_a[duplicated(tdat_a$outcome)==F,]
tdat_c <- merge(tdat_c,tdat_c_negcon,by="outcome",all=T)
tdat_c <-tdat_c[duplicated(tdat_c$outcome)==F,]

tdat_s[tdat_s==(-1)]<-"negative\nassociation"
tdat_s[tdat_s==(1)]<-"positive\nassociation"
tdat_c[tdat_c==(-1)]<-"negative\nassociation"
tdat_c[tdat_c==(1)]<-"positive\nassociation"
tdat_a[tdat_a==(-1)]<-"negative\nassociation"
tdat_a[tdat_a==(1)]<-"positive\nassociation"

## plot

## heatmap

tdat_s$total <- "1 line\nof evidence"
tdat_s$total[tdat_s$signmvr==tdat_s$signgrs]<-"2 lines\nof evidence"
tdat_s$total[tdat_s$signmvr==tdat_s$signnegcon]<-"2 lines\nof evidence"
tdat_s$total[tdat_s$signgrs==tdat_s$signnegcon]<-"2 lines\nof evidence"
tdat_s$total[(tdat_s$signgrs==tdat_s$signnegcon)&(tdat_s$signmvr==tdat_s$signnegcon)]<-"3 lines\nof evidence"

tdat_a$total <- "1 line\nof evidence"
tdat_a$total[tdat_a$signmvr==tdat_a$signgrs]<-"2 lines\nof evidence"
tdat_a$total[tdat_a$signmvr==tdat_a$signnegcon]<-"2 lines\nof evidence"
tdat_a$total[tdat_a$signgrs==tdat_a$signnegcon]<-"2 lines\nof evidence"
tdat_a$total[(tdat_a$signgrs==tdat_a$signnegcon)&(tdat_a$signmvr==tdat_a$signnegcon)]<-"3 lines\nof evidence"

tdat_c$total <- "1 line\nof evidence"
tdat_c$total[tdat_c$signmvr==tdat_c$signgrs]<-"2 lines\nof evidence"
tdat_c$total[tdat_c$signmvr==tdat_c$signnegcon]<-"2 lines\nof evidence"
tdat_c$total[tdat_c$signgrs==tdat_c$signnegcon]<-"2 lines\nof evidence"
tdat_c$total[(tdat_c$signgrs==tdat_c$signnegcon)&(tdat_c$signmvr==tdat_c$signnegcon)]<-"3 lines\nof evidence"

tdat_s_melt <- reshape2::melt(tdat_s,na.rm = F,id.vars="outcome")
tdat_s_melt$exposure<-"smoking"
tdat_s_melt$parent<-"partner"
tdat_c_melt <- reshape2::melt(tdat_c,na.rm = F,id.vars="outcome")
tdat_c_melt$exposure<-"caffeine consumption"
tdat_c_melt$parent<-"partner"
tdat_a_melt <- reshape2::melt(tdat_a,na.rm = F,id.vars="outcome")
tdat_a_melt$exposure<-"alcohol consumption"
tdat_a_melt$parent<-"partner"
tdat_partners <- rbind(tdat_s_melt,tdat_c_melt,tdat_a_melt)
tdat_partners <- tdat_partners[is.na(tdat_partners$outcome)==F,]

tdat_partners$outcome_class <- unlist(lapply(lapply(lapply(tdat_partners$outcome,stringr::str_split,pattern = "_"),unlist),"[",1))
tdat_partners$outcome_subclass <- paste(unlist(lapply(lapply(lapply(tdat_partners$outcome,stringr::str_split,pattern = "_"),unlist),"[",3)),
                                       unlist(lapply(lapply(lapply(tdat_partners$outcome,stringr::str_split,pattern = "_"),unlist),"[",4)),
                                       sep=" at ") 

tdat_partners$value <- factor(tdat_partners$value,ordered=T,levels=c("3 lines\nof evidence","2 lines\nof evidence","1 line\nof evidence","negative\nassociation","positive\nassociation"))
tdat_partners$exposure <- factor(tdat_partners$exposure,ordered=T,levels=c("smoking","alcohol consumption","caffeine consumption"))
tdat_partners<-tdat_partners[order(tdat_partners$outcome_class,tdat_partners$outcome_subclass),]
tdat_partners$outcome_subclass <- factor(tdat_partners$outcome_subclass,ordered=T,levels=unique(tdat_partners$outcome_subclass))

tdat_partners$variable2<-NA
tdat_partners$variable2[tdat_partners$variable=="signmvr"]<-"MVR"
tdat_partners$variable2[tdat_partners$variable=="signgrs"]<-"GRS"
tdat_partners$variable2[tdat_partners$variable=="signnegcon"]<-"NC"
tdat_partners$variable2[tdat_partners$variable=="total"]<-"T"


cols <- c("negative\nassociation" = "#fc8d59", "positive\nassociation" = "#91bfdb", "1 line\nof evidence" = "#e5f5e0", "2 lines\nof evidence" = "#a1d99b", "3 lines\nof evidence"="#31a354")

library(ggplot2)
ggplot(tdat_partners,aes(x=variable2,y=outcome_subclass))+
  geom_tile(aes(fill=factor(value)))+
  scale_fill_manual(values = cols,na.translate=F)+
  facet_grid(.~exposure,scales = "free")+
  theme_classic()+
  theme(axis.text.y=element_text(size=6))



require(ggh4x)
require(ggnewscale)
both <- rbind(tdat_mothers,tdat_partners)

both$variable2 <- factor(both$variable2,ordered=T,levels=c("MVR","NC","GRS","T"))
both$T <- "No"
both$T[both$variable2=="T"]<-"Yes"

my_strips <- strip_nested(
  # Horizontal strips
  text_x = elem_list_text(colour = c("black", "black","white"),size=c(10,10,1)),
  by_layer_x = TRUE
)

ggplot(both,aes(x=variable2,y=outcome_subclass))+
  geom_tile(aes(fill=factor(value)))+
  scale_fill_manual(values = cols,na.translate=F)+
  facet_nested(outcome_class~exposure+parent+T,scales = "free",space="free",strip = my_strips)+
  theme_minimal()+
  guides(colour="none",fill = guide_legend(byrow = TRUE))+
  theme(axis.text=element_text(size=8),axis.title=element_blank(),
        panel.spacing.y = unit(0.1, "lines"),
        panel.spacing.x = unit(c(0.1,0.3,0.1,2,0.1,0.3,0.1,2,0.1,0.3,0.1), "lines"),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_line(linewidth=0.2),
        strip.text.y=element_text(size=0),
        strip.text.x=element_text(margin=margin(b=0,t=0)),
        legend.title=element_blank(),legend.text=element_text(size=8),
        legend.key.spacing.y = unit(1.5, "lines"),
        legend.margin=margin(0,0,0,0),
        legend.box.spacing = unit(0, "pt"))




