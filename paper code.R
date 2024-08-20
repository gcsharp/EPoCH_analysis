# EPoCH paper code

require(tidyverse)
require(reshape2)

setwd("~/OneDrive - University of Exeter/Projects/EPoCH/")

# correlate SEP with health behaviours
alspac <- readRDS("/Volumes/MRC-IEU-research/projects/ieu2/p5/015/working/data/alspac/alspac_pheno.rds")
bib <- readRDS("/Volumes/MRC-IEU-research/projects/ieu2/p5/015/working/data/bib/bib_pheno.rds")
mcs <- readRDS("/Volumes/MRC-IEU-research/projects/ieu2/p5/015/working/data/mcs/mcs_pheno.rds")

variable_strings <- "edu_|occup_|smoking_father_ever_pregnancy_binary|alcohol_father_ever_pregnancy_binary|caffeine_father_total_ever_pregnancy_continuous|smoking_mother_ever_pregnancy_binary|alcohol_mother_ever_pregnancy_binary|caffeine_mother_total_ever_pregnancy_continuous"
correlate_SEP <- function(cohort){
  df <- cohort[,grepl(variable_strings,colnames(cohort))]
  cor(df,use="pairwise.complete")
}

alspac_cor <- as.data.frame(melt(correlate_SEP(alspac)))
bib_cor <- as.data.frame(melt(correlate_SEP(bib)))
mcs_cor <- as.data.frame(melt(correlate_SEP(mcs)))
all_cor <-list(alspac_cor,bib_cor,mcs_cor)
names(all_cor) <- c("alspac","bib","mcs")

all_cor <- bind_rows(all_cor,.id = "cohort")
all_cor <- droplevels(all_cor[grepl("occup|edu",all_cor$Var1)&grepl("alcohol|smoking|caffeine",all_cor$Var2),])
all_cor <- all_cor[-which(all_cor$Var2 %in% c("smoking_father_ever_pregnancy_binary","alcohol_father_ever_pregnancy_binary","caffeine_father_total_ever_pregnancy_continuous")),]
all_cor <- all_cor[grepl("mreport",all_cor$Var2)==F,]
all_cor <- all_cor[grepl("highestlowest",all_cor$Var1)==F,]

all_cor$parent <- unlist(lapply(str_split(all_cor$Var2,pattern="_"),"[",2))
all_cor$behaviour <- unlist(lapply(str_split(all_cor$Var2,pattern="_"),"[",1))
all_cor$sep_parent <- unlist(lapply(str_split(all_cor$Var1,pattern="_"),"[",3))
all_cor$sep_type <- unlist(lapply(str_split(all_cor$Var1,pattern="_"),"[",2))


pdf("Paper/Figures/SEP_comparisons.pdf",width=5,height=5)
require(scales)
require(ggh4x)
ggplot(all_cor,aes(x=sep_parent,y=parent))+
  geom_tile(aes(fill=value))+
  geom_text(aes(label=round(value,2)))+
  scale_fill_gradient2(low = muted("blue"),
                    mid = "white",
                    high = muted("red"),
                    midpoint = 0,
                    name="correlation")+
  facet_nested(cohort+behaviour~sep_type,scales="free",space="free")+
  theme_minimal()+xlab("")+ylab("")+
  theme(strip.text.y=element_text(angle=0,hjust=0),ggh4x.facet.nestline = element_line())
dev.off()
##

dat <- readRDS("~/OneDrive - University of Exeter/Projects/EPoCH/EPoCH results app/data/rds/all_results_reduced.rds")

## Creating unstrat (subset of dat for unstratified analyses)

unstrat <-dat[-grep(dat$model_new_name,pattern="stratified"),]
unstrat$est_SDM <- unstrat$est
unstrat$est_SDM[unstrat$outcome_type=="binary"]<-unstrat$est[unstrat$outcome_type=="binary"]*0.5513

unstrat$hit_category <- NA
unstrat$hit_category[abs(unstrat$est_SDM)>=0.2 & unstrat$fdr<0.05]<-"abs(effect)>0.2 & FDR-adj P<0.05"
unstrat$hit_category[abs(unstrat$est_SDM)<0.2 & unstrat$fdr<0.05]<-"abs(effect)<0.2 & FDR-adj P<0.05"
unstrat$hit_category[abs(unstrat$est_SDM)<0.2 & unstrat$fdr>0.05]<-"abs(effect)<0.2 & FDR-adj P>0.05"
unstrat$hit_category[abs(unstrat$est_SDM)>=0.2 & unstrat$fdr>0.05]<-"abs(effect)>0.2 & FDR-adj P>0.05"
unstrat$hit_category <- factor(unstrat$hit_category,ordered=T,levels=c("abs(effect)<0.2 & FDR-adj P>0.05","abs(effect)>0.2 & FDR-adj P>0.05","abs(effect)<0.2 & FDR-adj P<0.05","abs(effect)>0.2 & FDR-adj P<0.05"))

unstrat$person_exposed2 <- NA
unstrat$person_exposed2[unstrat$person_exposed=="mother"]<-"M"
unstrat$person_exposed2[unstrat$person_exposed=="partner"]<-"P"

unstrat$exposure_time2 <- NA
unstrat$exposure_time2[unstrat$exposure_time %in% c("age at initiation","alcohol","caffeine","cessation","heaviness","initiation")]<-"GRS"
unstrat$exposure_time2[unstrat$exposure_time %in% c("early onset")]<-"<12y"
unstrat$exposure_time2[unstrat$exposure_time %in% c("ever in pregnancy")]<-"Ever in\npregnancy"
unstrat$exposure_time2[unstrat$exposure_time %in% c("preconception")]<-"Before\npregnancy"
unstrat$exposure_time2[unstrat$exposure_time %in% c("ever in life")]<-"Ever in\nlife"
unstrat$exposure_time2[unstrat$exposure_time %in% c("at study recruitment")]<-""
unstrat$exposure_time2[unstrat$exposure_time %in% c("first trimester")]<-"First\ntrimester"
unstrat$exposure_time2[unstrat$exposure_time %in% c("second trimester")]<-"Second\ntrimester"
unstrat$exposure_time2[unstrat$exposure_time %in% c("third trimester")]<-"Third\ntrimester"
unstrat$exposure_time2[unstrat$exposure_time %in% c("first two postnatal years")]<-"After\npregnancy"
unstrat$exposure_time2 <- factor(unstrat$exposure_time2,ordered=T,levels=c("GRS","<12y","Ever in\nlife","Before\npregnancy","Ever in\npregnancy","First\ntrimester","Second\ntrimester","Third\ntrimester","After\npregnancy",""))

unstrat$exposure_time3 <- NA
unstrat$exposure_time3[unstrat$exposure_time %in% c("age at initiation","alcohol","caffeine","cessation","heaviness","initiation")]<-"GRS"
unstrat$exposure_time3[unstrat$exposure_time %in% c("early onset")]<-"<12y"
unstrat$exposure_time3[unstrat$exposure_time %in% c("ever in pregnancy")]<-"Ever preg"
unstrat$exposure_time3[unstrat$exposure_time %in% c("preconception")]<-"Before preg"
unstrat$exposure_time3[unstrat$exposure_time %in% c("ever in life")]<-"Ever life"
unstrat$exposure_time3[unstrat$exposure_time %in% c("at study recruitment")]<-"Low SEP"
unstrat$exposure_time3[unstrat$exposure_time %in% c("first trimester")]<-"First trim"
unstrat$exposure_time3[unstrat$exposure_time %in% c("second trimester")]<-"Second trim"
unstrat$exposure_time3[unstrat$exposure_time %in% c("third trimester")]<-"Third trim"
unstrat$exposure_time3[unstrat$exposure_time %in% c("first two postnatal years")]<-"After preg"
unstrat$exposure_time3 <- factor(unstrat$exposure_time3,ordered=T,levels=c("GRS","<12y","Ever life","Before preg","Ever preg","First trim","Second trim","Third trim","After preg","Low SEP"))

unstrat$exposure_class2 <- NA
unstrat$exposure_class2[unstrat$exposure_class=="smoking"]<-"Smoking"
unstrat$exposure_class2[unstrat$exposure_class=="alcohol consumption"]<-"Alcohol consumption"
unstrat$exposure_class2[unstrat$exposure_class=="caffeine consumption"]<-"Caffeine consumption"
unstrat$exposure_class2[unstrat$exposure_class=="low socioeconomic position"]<-"Low SEP"
unstrat$exposure_class2<- factor(unstrat$exposure_class2,ordered=T,levels=c("Smoking","Alcohol consumption","Caffeine consumption","Low SEP"))

# Summarising sample sizes
S4TS1 <- unstrat %>% group_by(model_new_name,person_exposed) %>% summarise(n_analyses=length(total_n),n_cohorts_more_1 =sum(cohorts_n>1), pc_cohorts_more_1 =100*(sum(cohorts_n>1)/length(cohorts_n)),mean_n=mean(total_n),min_n=min(total_n),max_n=max(total_n))
unstrat[unstrat$model=="model2a",] %>% group_by(cohorts_n>1) %>% summarise(n_analyses=length(total_n), mean=mean(total_n),min=min(total_n),max=max(total_n))
unstrat[unstrat$model=="model2b",] %>% group_by(cohorts_n>1) %>% summarise(n_analyses=length(total_n), mean=mean(total_n),min=min(total_n),max=max(total_n))

write.csv(S4TS1,"~/OneDrive - University of Exeter/Projects/EPoCH/S4TS1.csv")

# Summarising sample characteristics 

table1_alspac <- read.csv("~/OneDrive - University of Exeter/Projects/EPoCH/EPoCH results app/data/cohorts/alspac.csv")
table1_mcs <- read.csv("~/OneDrive - University of Exeter/Projects/EPoCH/EPoCH results app/data/cohorts/mcs.csv")
table1_bib <- read.csv("~/OneDrive - University of Exeter/Projects/EPoCH/EPoCH results app/data/cohorts/bib.csv")
cohorts <- list(table1_alspac,table1_bib,table1_mcs)
names(cohorts)<-c("alspac","bib","mcs")
S4TS2 <- bind_rows(cohorts,.id="cohort")
S4TS2 <- S4TS2[grepl("mother|partner",S4TS2$variable)&grepl("binary",S4TS2$variable)&grepl("alcohol|caffeine|smoking|low sep",S4TS2$variable),] %>% group_by(paste(variable, level)) %>% summarise(ncohorts=length(total.n),n=sum(total.n)-sum(n.missing),nlevel=sum(n.in.level))
S4TS2$pclevel <-100*(S4TS2$nlevel/S4TS2$n)
S4TS2 <- S4TS2[grepl("binary 0|binge",S4TS2$`paste(variable, level)`)==F,]
names(S4TS2)<-c("variable","ncohorts","n","nlevel","pclevel")
S4TS2$exposure_class <- unlist(lapply(str_split(S4TS2$variable,pattern = " - "),"[",1))
S4TS2$exposure_time <- unlist(lapply(str_split(S4TS2$variable,pattern = " - "),"[",2))
S4TS2$parent <- unlist(lapply(str_split(S4TS2$variable,pattern = " - "),"[",3))
S4TS2$exposure_class[S4TS2$exposure_class=="any caffeine"]<-"caffeine"
S4TS2$exposure_time[S4TS2$exposure_class=="low sep based on education"]<-"education"
S4TS2$exposure_time[S4TS2$exposure_class=="low sep based on occupation"]<-"occupation"
S4TS2$exposure_class[S4TS2$exposure_class=="low sep based on education"]<-"low SEP"
S4TS2$exposure_class[S4TS2$exposure_class=="low sep based on occupation"]<-"low SEP"
S4TS2$exposure_time<-factor(S4TS2$exposure_time,ordered=T,levels=c("education","occupation","ever in life","early onset (before age 12)","preconception","first trimester","second trimester","third trimester","ever in pregnancy","first two postnatal years"))

pdf(file = "OneDrive - University of Exeter/Projects/EPoCH/Paper/Figures/Barchart.pdf",width=7.5,height=6.5)

ggplot(S4TS2,aes(x=exposure_time,y=pclevel))+
  geom_col(fill="aquamarine4")+
  geom_text(size=3,aes(y=pclevel+6,label=paste0(round(pclevel),"%")))+
  coord_flip()+
  facet_grid(exposure_class~parent,space = "free",scales="free")+
  xlab("")+ylab("Percentage in exposed group")+
  theme_classic()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                        strip.background = element_rect(fill = "grey90",colour="white"),
                        strip.placement = "inside") 

dev.off()

## Function to make cohort summaries
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

## Combine and change to wide format
all_cohort_summaries <- rbind(alspac_summary,bib_summary,mcs_summary,moba_summary,total_summary)
all_cohort_summaries$exposure_dose <- paste0(all_cohort_summaries$exposure," (",all_cohort_summaries$dose,")")
all_cohort_summaries$value <- paste0(all_cohort_summaries$max_exposed,"/",all_cohort_summaries$max,
       " (",all_cohort_summaries$percent_exposed,"%)")
all_cohort_summaries$parent_time <- paste0(all_cohort_summaries$parent,": ",all_cohort_summaries$time)

require(tidyverse)
WIDE <- all_cohort_summaries[,c("cohort","exposure_dose","parent","time","value")] %>%
pivot_wider(names_from = c(parent), values_from = c(value))

## Save

write.csv(all_cohort_summaries,file="cohort_summaries_n_long.csv")
write.csv(WIDE,file="cohort_summaries_n.csv")

# Summarising effect sizes and p-values

reverse_vars <- c("verbal intelligence","total intelligence","spatial awareness",
                  "school readiness","reading skills","problem solving","performance intelligence",
                  "number skills","naming vocabulary","prosocial behaviour","birthweight")
unstrat$direction <- sign(unstrat$est)
unstrat$direction[unstrat$outcome_subclass2 %in% reverse_vars] <- unstrat$direction[unstrat$outcome_subclass2 %in% reverse_vars]*-1


## summarising hits by outcome and exposure class and time, model 2a

require(ggh4x)

df1 <- unstrat[unstrat$model=="model2a",c("exposure_class2","person_exposed","exposure_time3","hit_category","direction","outcome_class")] %>%
  group_by(direction,person_exposed,exposure_class2,exposure_time3,outcome_class,hit_category) %>%
  summarise(count=length(hit_category))

df2 <- unstrat[unstrat$model=="model2a",c("exposure_class2","person_exposed","exposure_time3","outcome_class")] %>%
  group_by(person_exposed,exposure_class2,exposure_time3,outcome_class) %>%
  summarise(count=length(outcome_class))

df <- left_join(df1,df2,by=c("exposure_class2","person_exposed","exposure_time3","outcome_class"))

df$percent <- 100*(df$count.x/df$count.y)
df$percent_sign <- df$percent*df$direction

pdf(file = "~/OneDrive - University of Exeter/Projects/EPoCH/Paper/Figures/Percentage_hits_all.pdf",width=12,height=8.5)

ggplot(df,aes(x=percent_sign,y=person_exposed))+
  geom_col(aes(fill=hit_category))+
  geom_vline(xintercept = 0)+
  scale_fill_manual(values=c("#c2a5cf","#a6dba0","#7b3294","#008837"))+
  facet_nested(exposure_class2+outcome_class~exposure_time3)+
  ylab("")+xlab("%")+
  labs(tag = "<-- associated with better child outcomes | associated with worse child outcomes -->") +
  theme_minimal()+theme(strip.text.y = element_text(angle=0,hjust=0),
                        axis.text.x=element_text(size=6,angle=90,hjust=1),
                        panel.spacing.x = unit(0.1,"lines"),panel.spacing.y = unit(0.2,"lines"),
                        legend.position = "bottom", legend.title=element_blank(),
                        ggh4x.facet.nestline = element_line(),
                        plot.tag.position = c(0.41,0.06),
                        plot.tag = element_text(size=12),
                        panel.grid.minor.y = element_blank(),
                        panel.grid.major.y=element_blank())

dev.off()


## summarising hits by exposure class and time

df1 <- unstrat[unstrat$model=="model2a",c("exposure_class2","person_exposed2","exposure_time3","hit_category","direction","outcome_class")] %>%
  group_by(direction,person_exposed2,exposure_class2,exposure_time3,hit_category) %>%
  summarise(count=length(hit_category))

df2 <- unstrat[unstrat$model=="model2a",c("exposure_class2","person_exposed2","exposure_time3","outcome_class")] %>%
  group_by(person_exposed2,exposure_class2,exposure_time3) %>%
  summarise(count=length(exposure_time3))

df <- left_join(df1,df2,by=c("exposure_class2","person_exposed2","exposure_time3"))

df$percent <- 100*(df$count.x/df$count.y)
df$percent_sign <- df$percent*df$direction

pdf(file = "~/OneDrive - University of Exeter/Projects/EPoCH/Paper/Figures/Percentage_hits_expclass_exptime.pdf",width=17,height=4)

ggplot(df,aes(y=percent_sign,x=person_exposed2))+
  geom_col(aes(fill=hit_category))+
  geom_hline(yintercept = 0)+
  scale_fill_manual(values=c("#c2a5cf","#a6dba0","#7b3294","#008837"))+
  facet_nested(.~exposure_class2+exposure_time3)+
  xlab("")+ylab("%")+
  labs(tag = "<-- associated with better child outcomes | associated with worse child outcomes -->") +
  theme_minimal()+theme(strip.text.y = element_text(angle=0,hjust=0),
                        panel.spacing.y = unit(0.1,"lines"),panel.spacing.x = unit(0.2,"lines"),
                        legend.position = "bottom", legend.title=element_blank(),
                        ggh4x.facet.nestline = element_line(),
                        plot.tag.position = c(-0.009,0.5),
                        plot.tag = element_text(size=6.5,angle=90),
                        panel.grid.minor.x = element_blank(),
                        panel.grid.major.x=element_blank(),
                        plot.margin = unit(c(0,0.5,0,0.6), "cm"))
dev.off()

## summarising hits by exposure class and outcome class

df1 <- unstrat[unstrat$model=="model2a",c("exposure_class2","person_exposed2","exposure_time3","hit_category","direction","outcome_class")] %>%
  group_by(direction,person_exposed2,exposure_class2,outcome_class,hit_category) %>%
  summarise(count=length(hit_category))

df2 <- unstrat[unstrat$model=="model2a",c("exposure_class2","person_exposed2","exposure_time3","outcome_class")] %>%
  group_by(person_exposed2,exposure_class2,outcome_class) %>%
  summarise(count=length(exposure_class2))

df <- left_join(df1,df2,by=c("exposure_class2","person_exposed2","outcome_class"))

df$percent <- 100*(df$count.x/df$count.y)
df$percent_sign <- df$percent*df$direction

pdf(file = "~/OneDrive - University of Exeter/Projects/EPoCH/Paper/Figures/Percentage_hits_expclass_outclass.pdf",width=6,height=7.5)

ggplot(df,aes(y=percent_sign,x=person_exposed2))+
  geom_col(aes(fill=hit_category))+
  geom_hline(yintercept = 0)+
  scale_fill_manual(values=c("#c2a5cf","#a6dba0","#7b3294","#008837"))+
  facet_nested(exposure_class2~outcome_class)+
  xlab("")+ylab("%")+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  labs(tag = "<-- associated with better child outcomes | associated with worse child outcomes -->") +
  theme_minimal()+theme(strip.text.y = element_text(angle=0,hjust=0),
                        panel.spacing.y = unit(0.1,"lines"),panel.spacing.x = unit(0.2,"lines"),
                        legend.position = "bottom", legend.title=element_blank(),
                        ggh4x.facet.nestline = element_line(),
                        plot.tag.position = c(-0.02,0.5),
                        plot.tag = element_text(size=8,angle=90),
                        panel.grid.minor.x = element_blank(),
                        panel.grid.major.x=element_blank(),
                        strip.text.x=element_text(angle=90,hjust=0),
                        plot.margin = unit(c(0,0.5,0,0.6), "cm"))
dev.off()


## summarising hits by outcome class and outcome time

df1 <- unstrat[unstrat$model=="model2a",c("exposure_class2","person_exposed2","exposure_time3","hit_category","direction","outcome_class","outcome_time")] %>%
  group_by(direction,person_exposed2,outcome_time,outcome_class,hit_category) %>%
  summarise(count=length(hit_category))

df2 <- unstrat[unstrat$model=="model2a",c("exposure_class2","person_exposed2","exposure_time3","outcome_class","outcome_time")] %>%
  group_by(person_exposed2,outcome_time,outcome_class) %>%
  summarise(count=length(outcome_class))

df <- left_join(df1,df2,by=c("outcome_time","person_exposed2","outcome_class"))

df$percent <- 100*(df$count.x/df$count.y)
df$percent_sign <- df$percent*df$direction

df$outcome_time <- factor(df$outcome_time, ordered=T,levels=c("delivery","first year","age 1 to 2","age 3 to 4","age 5 to 7","age 8 to 11", "any time in childhood"))

pdf(file = "~/OneDrive - University of Exeter/Projects/EPoCH/Paper/Figures/Percentage_hits_outtime_outclass.pdf",width=6,height=9)

ggplot(df,aes(y=percent_sign,x=person_exposed2))+
  geom_col(aes(fill=hit_category))+
  geom_hline(yintercept = 0)+
  scale_fill_manual(values=c("#c2a5cf","#a6dba0","#7b3294","#008837"))+
  facet_nested(outcome_class~outcome_time)+
  xlab("")+ylab("%")+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  labs(tag = "<-- associated with better child outcomes | associated with worse child outcomes -->") +
  theme_minimal()+theme(strip.text.y = element_text(angle=0,hjust=0),
                        panel.spacing.y = unit(0.1,"lines"),panel.spacing.x = unit(0.2,"lines"),
                        legend.position = "bottom", legend.title=element_blank(),
                        ggh4x.facet.nestline = element_line(),
                        plot.tag.position = c(-0.02,0.5),
                        plot.tag = element_text(size=8,angle=90),
                        panel.grid.minor.x = element_blank(),
                        panel.grid.major.x=element_blank(),
                        strip.text.x=element_text(angle=90,hjust=0),
                        plot.margin = unit(c(0,0.5,0,0.6), "cm"))
dev.off()

## summarising hits by model

df1 <- unstrat[,c("exposure_class2","person_exposed2","exposure_time3","hit_category","direction","outcome_class","outcome_time","model")] %>%
  group_by(direction,person_exposed2,exposure_class2,model,hit_category) %>%
  summarise(count=length(hit_category))

df2 <- unstrat[,c("exposure_class2","person_exposed2","exposure_time3","outcome_class","outcome_time","model")] %>%
  group_by(person_exposed2,model,exposure_class2) %>%
  summarise(count=length(exposure_time3))

df <- left_join(df1,df2,by=c("model","person_exposed2","exposure_class2"))

df$percent <- 100*(df$count.x/df$count.y)
df$percent_sign <- df$percent*df$direction

pdf(file = "~/OneDrive - University of Exeter/Projects/EPoCH/Paper/Figures/Percentage_hits_model.pdf",width=6,height=7)

ggplot(df,aes(y=percent_sign,x=person_exposed2))+
  geom_col(aes(fill=hit_category))+
  geom_hline(yintercept = 0)+
  scale_fill_manual(values=c("#c2a5cf","#a6dba0","#7b3294","#008837"))+
  facet_nested(exposure_class2~model)+
  xlab("")+ylab("%")+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  labs(tag = "<-- associated with better child outcomes | associated with worse child outcomes -->") +
  theme_minimal()+theme(strip.text.y = element_text(angle=0,hjust=0),
                        panel.spacing.y = unit(0.1,"lines"),panel.spacing.x = unit(0.2,"lines"),
                        legend.position = "bottom", legend.title=element_blank(),
                        ggh4x.facet.nestline = element_line(),
                        plot.tag.position = c(-0.02,0.5),
                        plot.tag = element_text(size=8,angle=90),
                        panel.grid.minor.x = element_blank(),
                        panel.grid.major.x=element_blank(),
                        strip.text.x=element_text(angle=90,hjust=0),
                        plot.margin = unit(c(0,0.5,0,0.6), "cm"))
dev.off()

## distribution of effect estimates and p-values for mothers and partners

calculate_hit_counts <- function(model,dat){
  D <- dat[dat$model == model,]
  
  Counts <- c(
    "abs(effect)<0.2 & FDR-adj P>0.05" = length(D$hit_category[D$hit_category=="abs(effect)<0.2 & FDR-adj P>0.05"]),
    "abs(effect)>0.2 & FDR-adj P>0.05" = length(D$hit_category[D$hit_category=="abs(effect)>0.2 & FDR-adj P>0.05"]),
    "abs(effect)<0.2 & FDR-adj P<0.05" = length(D$hit_category[D$hit_category=="abs(effect)<0.2 & FDR-adj P<0.05"]),
    "abs(effect)>0.2 & FDR-adj P<0.05" =  length(D$hit_category[D$hit_category=="abs(effect)>0.2 & FDR-adj P<0.05"]),
    "abs(effect)<0.2 & FDR-adj P>0.05" = length(D$hit_category[D$hit_category=="abs(effect)<0.2 & FDR-adj P>0.05"&D$person_exposed=="mother"]),
    "abs(effect)>0.2 & FDR-adj P>0.05" = length(D$hit_category[D$hit_category=="abs(effect)>0.2 & FDR-adj P>0.05"&D$person_exposed=="mother"]),
    "abs(effect)<0.2 & FDR-adj P<0.05" = length(D$hit_category[D$hit_category=="abs(effect)<0.2 & FDR-adj P<0.05"&D$person_exposed=="mother"]),
    "abs(effect)>0.2 & FDR-adj P<0.05" =  length(D$hit_category[D$hit_category=="abs(effect)>0.2 & FDR-adj P<0.05"&D$person_exposed=="mother"]),
    "abs(effect)<0.2 & FDR-adj P>0.05" = length(D$hit_category[D$hit_category=="abs(effect)<0.2 & FDR-adj P>0.05"&D$person_exposed=="partner"]),
    "abs(effect)>0.2 & FDR-adj P>0.05" = length(D$hit_category[D$hit_category=="abs(effect)>0.2 & FDR-adj P>0.05"&D$person_exposed=="partner"]),
    "abs(effect)<0.2 & FDR-adj P<0.05" = length(D$hit_category[D$hit_category=="abs(effect)<0.2 & FDR-adj P<0.05"&D$person_exposed=="partner"]),
    "abs(effect)>0.2 & FDR-adj P<0.05" =  length(D$hit_category[D$hit_category=="abs(effect)>0.2 & FDR-adj P<0.05"&D$person_exposed=="partner"]))
  Proportions <- c(Counts[1]/sum(Counts[1:4],na.rm=T),Counts[2]/sum(Counts[1:4],na.rm=T),Counts[3]/sum(Counts[1:4],na.rm=T),Counts[4]/sum(Counts[1:4],na.rm=T),
                   Counts[5]/sum(Counts[5:8],na.rm=T),Counts[6]/sum(Counts[5:8],na.rm=T),Counts[7]/sum(Counts[5:8],na.rm=T),Counts[8]/sum(Counts[5:8],na.rm=T),
                   Counts[9]/sum(Counts[9:12],na.rm=T),Counts[10]/sum(Counts[9:12],na.rm=T),Counts[11]/sum(Counts[9:12],na.rm=T),Counts[12]/sum(Counts[9:12],na.rm=T))
  DF <- data.frame(category=names(Counts),
                   counts=Counts,
                   proportions=Proportions,
                   model=model,
                   parent=c(rep("Either",4),rep("Mother",4),rep("Partner",4)))
  DF
}

models = unique(unstrat$model)
exposureclasses=unique(unstrat$exposure_class)
exposuretimes=unique(unstrat$exposure_time)
outcomeclasses=unique(unstrat$outcome_class)

res <- lapply(exposureclasses,function(x){
  res <- lapply(models,calculate_hit_counts,dat=unstrat[unstrat$exposure_class==x,])
  res <- bind_rows(res)
  row.names(res)<-NULL
  res$exposure_class<-x
  res
})
res <- bind_rows(res)

res$category <- factor(res$category,ordered=T,levels=c("abs(effect)<0.2 & FDR-adj P>0.05","abs(effect)>0.2 & FDR-adj P>0.05","abs(effect)<0.2 & FDR-adj P<0.05","abs(effect)>0.2 & FDR-adj P<0.05"))

res$exposure_class2 <- NA
res$exposure_class2[res$exposure_class=="smoking"]<-"Smoking"
res$exposure_class2[res$exposure_class=="alcohol consumption"]<-"Alcohol consumption"
res$exposure_class2[res$exposure_class=="caffeine consumption"]<-"Caffeine consumption"
res$exposure_class2[res$exposure_class=="low socioeconomic position"]<-"Low SEP"
res$exposure_class2<- factor(res$exposure_class2,ordered=T,levels=c("Smoking","Alcohol consumption","Caffeine consumption","Low SEP"))

## Text for main body:
res %>% group_by(category,parent) %>% summarise(mean(proportions,na.rm=T))
res[res$model=="model2b",] %>% group_by(category,parent) %>% summarise(mean(proportions,na.rm=T))
res[res$model=="model2b"&res$parent=="Either",] %>% group_by(category,exposure_class,parent) %>% summarise(mean(proportions,na.rm=T))
prop.table(table(unstrat$hit_category[unstrat$model=="model2b"&unstrat$exposure_time3=="GRS"]))

# Creating MatPat (df containing maternal and corresponding paternal analysis in wide format)

mat <- unstrat[unstrat$person_exposed=="mother",]
pat <- unstrat[unstrat$person_exposed=="partner",]
matpat <- merge(mat,pat,by=c("exposure_class2","exposure_subclass","exposure_time2","exposure_type","exposure_dose","outcome_linker","model"),all=T,suffixes = c("mat","pat"))
matpat$samedir <- sign(matpat$estmat)==sign(matpat$estpat)
matpat$samedir_diff <- abs(matpat$estmat) - abs(matpat$estpat)
matpat$samedir_diff[matpat$samedir==F]<-NA
matpat$samedir_pc_change <- (abs(matpat$samedir_diff)/abs(matpat$estmat))*100

# Correlating maternal and paternal effects

df <- droplevels(matpat[matpat$model=="model2b",])
df <- droplevels(df[-which(is.na(df$est_SDMmat)|is.na(df$est_SDMpat)),])

require(tidymodels)

gen_scatter_plot <- function(class_time){
  Class <- unlist(lapply(strsplit(class_time,split="SPLIT"),"[",1))
  Time <- unlist(lapply(strsplit(class_time,split="SPLIT"),"[",2))
  Colour <- NA
  if(Class=="Smoking"){Colour <-"#1b9e77"}
  if(Class=="Alcohol consumption"){Colour <-"#d95f02"}
  if(Class=="Caffeine consumption"){Colour <-"#7570b3"}
  if(Class=="Low SEP"){Colour <-"#e7298a"}
  df2 <- df[df$exposure_class2==Class&df$exposure_time3mat==Time,]
  if(Class=="Low SEP"){ df2 <- df[df$exposure_class2==Class,]}
  Title <- paste0(Class,"\n",Time)
  if(Class=="Low SEP"){Title <- paste0("Low SEP","\n"," ")}
P <- ggplot(df2,aes(x=est_SDMmat,y=est_SDMpat))+
  geom_abline(slope=1,intercept=0,colour="grey80",linetype="dashed")+
  geom_point(alpha=0.1,colour=Colour)+
  geom_smooth(method='lm',se=F,linewidth=0.5,colour=Colour)+
  scale_color_brewer(palette = "Dark2")+
#  facet_wrap(.~exposure_time2,drop = T,scales="free",nrow=1)+
  coord_obs_pred()+
  theme_classic()+
#  xlim(c(-2,2))+ylim(c(-2,2))+
  xlab("Mothers")+ylab("Partners")+
  theme(strip.background = element_blank(),ggh4x.facet.nestline = element_line(),
        legend.position="none",aspect.ratio = 1,plot.title = element_text(hjust = 0.5,size=8),axis.title = element_text(size=8),
        plot.margin = unit(c(0,0.2,0,1), 'lines'),axis.text=element_text(size=4))+
  ggtitle(Title)
P
}

df$class_time <- paste0(df$exposure_class2,"SPLIT",df$exposure_time3mat)
Plots <- lapply(c(unique(df$class_time)[1:10],unique(df$class_time)[10:19]),gen_scatter_plot)
Plots[[10]]<- Plots[[10]]+theme_void()+theme(plot.title=element_blank(),legend.position = "none")+  geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,fill="white",colour="white")

require(ggpubr)

Plots <- ggarrange(plotlist=Plots, ncol = 10, nrow=2,align="v")
Plots

df$class_time <- paste0(df$exposure_class2,"SPLIT",df$exposure_time2)
library(dplyr)
correlation_by_group <- df %>%
  group_by(class_time) %>%
  summarise(correlation = cor(est_SDMmat, est_SDMpat))
correlation_by_group$class <- unlist(lapply(strsplit(correlation_by_group$class_time,split="SPLIT"),"[",1))
correlation_by_group$time <- unlist(lapply(strsplit(correlation_by_group$class_time,split="SPLIT"),"[",2))
correlation_by_group$class_time<-NULL
correlation_by_group$time[is.na(correlation_by_group$time)]<-""
require(reshape2)
df_corr <- dcast(correlation_by_group, time~class,value.var = 'correlation')
row.names(df_corr)<-df_corr$time
df_corr$time <- NULL
df_corr <- as.matrix(df_corr)
require(corrplot)
corrplot(df_corr,is.corr = F,method = "color",tl.col = "grey30",tl.cex = 1)

# Strongest associations - Model 2B upturned manhattan overview
mt_m <- unstrat[unstrat$model=="model2b",c("outcome_type","person_exposed","exposure_class2","exposure_subclass","exposure_time3","exposure_dose","exposure_type","outcome_class","outcome_subclass2","outcome_time","est","est_SDM","fdr","outcome_subclass1","p","hit_category")]
mt_m$exposure <- paste(mt_m$exposure_class2,mt_m$exposure_subclass,mt_m$exposure_time3,mt_m$exposure_dose,mt_m$exposure_type)
mt_m$outcome <- paste(mt_m$outcome_class,mt_m$outcome_subclass2,mt_m$outcome_time)
mt_m$exposure_class <- factor(mt_m$exposure_class2, ordered=T, levels=c("Smoking","Alcohol consumption", "Caffeine consumption","Low SEP"))

pdf(file = "OneDrive - University of Exeter/Projects/EPoCH/Paper/Figures/Coefficients.pdf",width=12,height=10)

ggplot(mt_m,aes(y=outcome_subclass2,x=est_SDM,colour=hit_category))+
  geom_jitter(aes(alpha=as.numeric(hit_category)*0.25))+
  geom_vline(xintercept=0)+
 scale_colour_manual(values=c("#c2a5cf","#a6dba0","#7b3294","#008837"))+
  facet_nested(outcome_class~exposure_class+person_exposed,space = "free_y",scales="free_y")+
  theme_classic()+xlab("Standardised effect estimate (Cohen's D)")+ylab("")+
  guides(alpha = "none")+
  theme(strip.text.y=element_blank(),strip.background = element_blank(),ggh4x.facet.nestline = element_line(),
        legend.position="bottom",legend.title=element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

dev.off()


# triangulation with heatmap DURING PREGNANCY

DIM <- unstrat[unstrat$exposure_subclass!="genetic risk score"&unstrat$exposure_time%in%c("ever in pregnancy","first trimester","second trimester","third trimester")&unstrat$model=="model2a",c("person_exposed","exposure_class","exposure_subclass","exposure_time","exposure_type","exposure_dose","outcome_class","outcome_subclass1","outcome_subclass2","outcome_time","outcome_type","est","se","p","i2","hetp","cohorts","cohorts_n","total_n","fdr")] #model 2a for MVR,obs unstrata only
dim(DIM[DIM$person_exposed=="mother",])
dim(DIM[DIM$person_exposed=="partner",])
dim(DIM[DIM$person_exposed=="mother"&DIM$fdr<0.05,])
dim(DIM[DIM$person_exposed=="partner"&DIM$fdr<0.05,])

source("OneDrive - University of Exeter/Projects/EPoCH/EPoCH analysis/triangulation.R")

T_mothers_preg <-triangulate_results(unstrat,"mother",c("ever in pregnancy"),"postnatal")
T_partners_preg <-triangulate_results(unstrat,"partner",c("ever in pregnancy"),"postnatal")
T_both_preg<-combine_triang_results(T_mothers_preg,T_partners_preg)

T_mothers_precon_post <-triangulate_results(unstrat,"mother","preconception","postnatal")
T_partners_precon_post <-triangulate_results(unstrat,"partner","preconception","postnatal")
T_both_precon_post<-combine_triang_results(T_mothers_precon_post,T_partners_precon_post)

T_mothers_precon_preg <-triangulate_results(unstrat,"mother","preconception","during pregnancy")
T_partners_precon_preg <-triangulate_results(unstrat,"partner","preconception","during pregnancy")
T_both_precon_preg<-combine_triang_results(T_mothers_precon_preg,T_partners_precon_preg)

saveRDS(T_both_preg,"OneDrive - University of Exeter/Projects/EPoCH/EPoCH results app/data/triangulation/triangulation_pregnancy.rds")


# plot
require(reshape2)

my_strips <- strip_nested(
  # Horizontal strips
  text_x = elem_list_text(colour = c("black", "black","white"),size=c(10,10,1)),
  by_layer_x = TRUE
)

cols <- c("negative\nassociation" = "#fc8d59", "positive\nassociation" = "#91bfdb","0 lines\nof evidence" = "grey90", "1 line\nof evidence" = "#edf8e9", "2 lines\nof evidence" = "#bae4b3", "3 lines\nof evidence"="#74c476", "4 lines\nof evidence"="#31a354", "5 lines\nof evidence"="#006d2c")

# removing outcomes from plot if no MVR association has FDR<0.05
FDR_insig <- T_both_preg[is.na(T_both_preg$value) & T_both_preg$variable=="signmvr2",]
FDR_insig_t <-data.frame(table(FDR_insig$outcome,paste(FDR_insig$exposure,FDR_insig$parent)))
FDR_insig_w <- dcast(FDR_insig_t, Var1~Var2,value.var = 'Freq')
FDR_insig_w$all_insig <- rowSums(FDR_insig_w[,-1])==6
FDR_insig <- as.character(FDR_insig_w$Var1[FDR_insig_w$all_insig])

data_to_plot <- droplevels(T_both_preg[(T_both_preg$outcome %in% FDR_insig)==F,])

pdf(file = "OneDrive - University of Exeter/Projects/EPoCH/Paper/Figures/Triangulation.pdf",width=10,height=17)

ggplot(data_to_plot,aes(x=variable2,y=outcome_subclass))+
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

dev.off()

T_both_preg[which(T_both_preg$value=="5 lines\nof evidence"),]->Pregnancy_outcomes
T_both_precon_preg[which(T_both_precon_preg$value=="5 lines\nof evidence"),]->Precon_Preg_outcomes
T_both_precon_post[which(T_both_precon_post$value=="5 lines\nof evidence"),]->Postcon_Preg_outcomes

# now compare these with effect of SEP to contextualise evidence

apply(Pregnancy_outcomes,1,paste,collapse="_")[apply(Pregnancy_outcomes,1,function(x){paste(x,collapse="_") %in% apply(Prenatal_outcomes,1,paste,collapse="_")})]




