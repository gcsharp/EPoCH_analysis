# CREATES: "N:/durable/projects/EPoCH/pheno_and_gen.rds"

############################
##     Set up workspace   ##
############################
# For installing packages available on CRAN, use:
#install.packages("dplyr", repos = "file://tsd-evs/shared/R/cran")
#install.packages("tidyverse", repos = "file://tsd-evs/shared/R/cran")
library(dplyr)
library(foreign)
library(tidyverse)
library(haven)

############################
####     Read in data   ####
############################
q1 <- read.spss("N:/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_Q1_v12.sav", to.data.frame = TRUE)
 q2 <- read.spss("N:/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_Q2CDW_v12.sav", to.data.frame = TRUE)
 q3 <- read.spss("N:/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_Q3_v12.sav", to.data.frame = TRUE)
 q6m <- read.spss("N:/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_Q4_6months_v12.sav", to.data.frame = TRUE)
 q18m <- read.spss("N:/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_Q5_18months_v12.sav", to.data.frame = TRUE)
 q3yr <- read.spss("N:/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_Q6_3yrs_v12.sav", to.data.frame = TRUE) 
 q5yr <- read.spss("N:/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_Q5yrs_v12.sav", to.data.frame = TRUE)
q7yr <- read.spss("N:/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_Q7yrs_v12.sav", to.data.frame = TRUE)
 q8yr <- read.spss("N:/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_Q8yrs_v12.sav", to.data.frame = TRUE)
 qfar <- read.spss("N:/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_QF_v12.sav", to.data.frame = TRUE)
 mbrn <- read.spss("N:/durable/data/MoBaPhenoData/PDB2306_MoBa_V12/SPSS/PDB2306_MBRN_541_v12.sav", to.data.frame = TRUE) 

## joins by PREG_ID_2306 (for parents) and BARN_NR (for children)
parents <- merge(q1,q2,by="PREG_ID_2306",all=T)
parents <- merge(parents,q3,by="PREG_ID_2306",all=T)
parents <- merge(parents,qfar,by="PREG_ID_2306",all=T)

children <- merge(q6m,q18m,by=c("PREG_ID_2306","BARN_NR"),all=T)
children <- merge(children,q3yr,by=c("PREG_ID_2306","BARN_NR"),all=T)
children <- merge(children,q5yr,by=c("PREG_ID_2306","BARN_NR"),all=T)
children <- merge(children,q7yr,by=c("PREG_ID_2306","BARN_NR"),all=T)
children <- merge(children,q8yr,by=c("PREG_ID_2306","BARN_NR"),all=T)
children <- merge(mbrn,children,by=c("PREG_ID_2306","BARN_NR"),all=T)

raw_dat <- merge(parents,children,by=c("PREG_ID_2306"),all=T)

#save raw files
saveRDS(raw_dat,"N:/durable/projects/EPoCH/raw_dat2.rds")

# if need to load raw dataset at different time
raw_dat <- readRDS("N:/durable/projects/EPoCH/raw_dat2.rds")

# tidy up
rm(children, mbrn,parents, q1,q18m,q2,q3,q3yr,q5yr,q6m,q7yr,q8yr,qfar)

# create dat file to save new variables to:
dat<-raw_dat[,c("PREG_ID_2306","BARN_NR")]

# Sex (75 not specified, 40 uncertain)
dat$covs_sex<-NA
dat$covs_sex[raw_dat$KJONN=="Male"]<-"male"
dat$covs_sex[raw_dat$KJONN=="Female"]<-"female"

# Child ages (months)

dat$covs_age_child_stage0<-raw_dat$ALDERRETUR_S4/30 #stage 0 (first year)
dat$covs_age_child_stage0[dat$covs_age_child_stage0<0]<-NA
dat$covs_age_child_stage0[dat$covs_age_child_stage0>12]<-NA
dat$covs_age_child_stage1<-raw_dat$ALDERRETUR_S5/30 #stage 1 (1 to 2)
dat$covs_age_child_stage2<-raw_dat$ALDERRETUR_S6/30 #stage 2 (3 to 4)
dat$covs_age_child_stage3<-raw_dat$AGE_RETURN_MTHS_Q5AAR #stage 3 (5 to 7)
dat$covs_age_child_stage4<-raw_dat$AGE_RETURN_MTHS_Q8AAR #stage 4 (8 to 11)

# live births
dat$peri_live_birth_binary<-NA
dat$peri_live_birth_binary[raw_dat$DODKAT%in% c("Live born, still alive","Live born, died 0-2 years of age or >2 years of age", "Live born, unknown follow-up status, emigrated or registered birth certificate")]<-1
dat$peri_live_birth_binary[raw_dat$DODKAT=="Stillborn (died before/during delivery or unknown time of death) or Abortion (requiring approval (§2.3c))"]<-0

# participation
## creating control group of responded to Q1 (AA) questionnaire (haven't done for later Qs as havent needed them):
library(stringr)

dat$maternal_participation<-NA
dat$maternal_participation[str_replace_all(raw_dat$VERSJON_SKJEMA1_TBL1, " ", "") %in% c("SKJEMA1A", "SKJEMA1B", "SKJEMA1C", "SKJEMA1E")]<-1

dat$paternal_participation<-NA
dat$paternal_participation[str_replace_all(raw_dat$VERSJON_SKJEMAFAR_TBL1, " ", "") %in% c("FARB", "FARC", "FARD", "FARE")]<-1

#multiple pregnancies
duplicates<-data.frame(table(dat$PREG_ID_2306))
duplicates$PREG_ID_2306<-duplicates$Var1
duplicates$Var1 <- NULL

duplicates <- duplicates[duplicates$Freq>1,]

dat$multiple_pregnancy<-0
dat$multiple_pregnancy[dat$PREG_ID_2306 %in% duplicates$PREG_ID_2306]<-1 # is multiple

############################################
#######           Smoking            #######
############################################

### Maternal

  ## Here I have used ref categories of variables that say no the behaviour-and ones that say yes to show amount (else would just be comparing against a control of smokers)
  
#any time up to birth: ever vs never smoker:binary
dat$smoking_mother_ever_life_binary<-NA
dat$smoking_mother_ever_life_binary[raw_dat$AA1355=="Yes"]<-1
dat$smoking_mother_ever_life_binary[raw_dat$AA1355=="No"]<-0

#smoking before age 11 vs no smoking before age 11
dat$smoking_mother_before11_binary<-NA
dat$smoking_mother_before11_binary[raw_dat$AA1362<=11 & raw_dat$AA1362>0 & raw_dat$AA1362<70]<-1
dat$smoking_mother_before11_binary[raw_dat$AA1362>11&raw_dat$AA1362<=40]<-0


#preconception: number of cigarettes per day in the 3 months prior to pregnancy (aa1360 if sometimes, how many pwk?/ aa1361if daily, how many pd?)
dat$smoking_mother_preconception_ordinal<-NA
tmp_cigs_daily <- raw_dat$AA1361
tmp_cigs_sometimes <- raw_dat$AA1360/7 #divided by 7 because if it's sometimes, the question is n cigs pwk
tmp_cigs_pd <- apply(data.frame(tmp_cigs_sometimes,tmp_cigs_daily),1,max,na.rm = T)
tmp_cigs_pd[is.infinite(tmp_cigs_pd)]<-NA
dat$smoking_mother_preconception_ordinal[tmp_cigs_pd==0|raw_dat$AA1359=="No"]<-"None"
dat$smoking_mother_preconception_ordinal[tmp_cigs_pd>0&tmp_cigs_pd<10]<-"Light" #i.e. <10 but not 0
dat$smoking_mother_preconception_ordinal[tmp_cigs_pd<20 & tmp_cigs_pd>=10]<-"Moderate" # i.e. <20, >=10
dat$smoking_mother_preconception_ordinal[tmp_cigs_pd>=20]<-"Heavy" #i.e. greater than 19
dat$smoking_mother_preconception_ordinal<-factor(dat$smoking_mother_preconception_ordinal,levels=c("None","Light","Moderate","Heavy"), ordered=T)

#pre-conception: any vs no smoking in the first 3 months prior to pregnancy (AA1359 did you smoke sometimes/daily/no)
dat$smoking_mother_preconception_binary<-NA
dat$smoking_mother_preconception_binary[raw_dat$AA1359=="Sometimes" | raw_dat$AA1359=="Daily"|raw_dat$AA1359=="Sometimes + Daily"| raw_dat$AA1359=="No + Sometimes"|raw_dat$AA1359=="No + Daily"|raw_dat$AA1359=="No + Sometimes + Daily"|dat$smoking_mother_preconception_ordinal%in% c("Light","Moderate","Heavy")]<-1
dat$smoking_mother_preconception_binary[dat$smoking_mother_preconception_ordinal=="None"]<-0

# any vs no smoking in the first trimester(<12 weeks)
## NA

#number of cigarettes per day in the first trimester
 ## NA

#number of cigarettes per day in the second trimester (15 wks)
dat$smoking_mother_secondtrim_ordinal<-NA
tmp_cigs_daily <- raw_dat$AA1358
tmp_cigs_sometimes <- raw_dat$AA1357/7 #divided by 7 because if it's sometimes, the question is n cigs pwk
tmp_cigs_pd <- apply(data.frame(tmp_cigs_sometimes,tmp_cigs_daily),1,max,na.rm = T)
tmp_cigs_pd[is.infinite(tmp_cigs_pd)]<-NA
dat$smoking_mother_secondtrim_ordinal[tmp_cigs_pd==0|raw_dat$AA1356=="No"]<-"None"
dat$smoking_mother_secondtrim_ordinal[tmp_cigs_pd>0&tmp_cigs_pd<10]<-"Light" #i.e. <10 but not 0
dat$smoking_mother_secondtrim_ordinal[tmp_cigs_pd<20 & tmp_cigs_pd>=10]<-"Moderate" # i.e. <20, >=10
dat$smoking_mother_secondtrim_ordinal[tmp_cigs_pd>=20]<-"Heavy" #i.e. greater than 19
dat$smoking_mother_secondtrim_ordinal<-factor(dat$smoking_mother_secondtrim_ordinal,levels=c("None","Light","Moderate","Heavy"), ordered=T)

# any vs no smoking in the second trimester
#(asked at 15 weeks)
dat$smoking_mother_secondtrim_binary<-NA
dat$smoking_mother_secondtrim_binary[raw_dat$AA1356=="Sometimes" | raw_dat$AA1356=="Daily"|raw_dat$AA1356=="Sometimes + Daily"| raw_dat$AA1356=="No + Sometimes"|raw_dat$AA1356=="No + Daily"|raw_dat$AA1356=="No + Sometimes + Daily"|dat$smoking_mother_secondtrim_ordinal%in% c("Light","Moderate","Heavy")]<-1
dat$smoking_mother_secondtrim_binary[dat$smoking_mother_secondtrim_ordinal=="None"]<-0

#number of cigarettes per day in the third trimester (27 wks)
dat$smoking_mother_thirdtrim_ordinal<-NA
tmp_cigs_daily <- raw_dat$CC1039
tmp_cigs_sometimes <- raw_dat$CC1038/7 #divided by 7 because if it's sometimes, the question is n cigs pwk
tmp_cigs_pd <- apply(data.frame(tmp_cigs_sometimes,tmp_cigs_daily,raw_dat$DD738,raw_dat$DD1114),1,max,na.rm = T)
tmp_cigs_pd[is.infinite(tmp_cigs_pd)]<-NA
tmp_cigs_pd_D <- 
dat$smoking_mother_thirdtrim_ordinal[tmp_cigs_pd==0|raw_dat$CC1037=="No"|raw_dat$DD732=="Did not smoke"]<-"None"
dat$smoking_mother_thirdtrim_ordinal[tmp_cigs_pd>0&tmp_cigs_pd<10]<-"Light" #i.e. <10 but not 0
dat$smoking_mother_thirdtrim_ordinal[tmp_cigs_pd<20 & tmp_cigs_pd>=10]<-"Moderate" # i.e. <20, >=10
dat$smoking_mother_thirdtrim_ordinal[tmp_cigs_pd>=20]<-"Heavy" #i.e. greater than 19
dat$smoking_mother_thirdtrim_ordinal<-factor(dat$smoking_mother_thirdtrim_ordinal,levels=c("None","Light","Moderate","Heavy"), ordered=T)

# any vs no smoking in the third trimester
#(asked at 27 weeks)
dat$smoking_mother_thirdtrim_binary<-NA
dat$smoking_mother_thirdtrim_binary[raw_dat$DD732 %in% c("Smoked sometimes","Smoked every day")|raw_dat$CC1037=="Sometimes" | raw_dat$CC1037=="Daily"|raw_dat$CC1037=="Sometimes + Daily"| raw_dat$CC1037=="No + Sometimes"|raw_dat$CC1037=="No + Daily"|raw_dat$CC1037=="No + Sometimes + Daily"|dat$smoking_mother_thirdtrim_ordinal%in% c("Light","Moderate","Heavy")]<-1
dat$smoking_mother_thirdtrim_binary[dat$smoking_mother_thirdtrim_ordinal=="None"]<-0

#Passive smoke during pregnancy (binary var)
dat$smoking_mother_passive_binary<-NA
dat$smoking_mother_passive_binary[which(raw_dat$AA1349=="Yes" |raw_dat$AA1351=="Yes" | raw_dat$CC1033=="Yes" | raw_dat$CC1035=="Yes")]<-1
dat$smoking_mother_passive_binary[which(raw_dat$AA1349=="No" & raw_dat$AA1351 =="No" & raw_dat$CC1033=="No" | raw_dat$CC1035=="No")]<-0

#postnatal smoking (ordered categorical up until age 2)
temp3<-NA
temp3[raw_dat$DD733=="Did not smoke"|raw_dat$DD739==0|raw_dat$DD1115==0]<-0
temp3[(raw_dat$DD739>0 & raw_dat$DD739<10)|(raw_dat$DD1115>0 & raw_dat$DD1115<10)]<-1
temp3[(raw_dat$DD739>9 & raw_dat$DD739<20)|(raw_dat$DD1115>9& raw_dat$DD1115<20)]<-2
temp3[raw_dat$DD739>=20|raw_dat$DD1115>=20]<-3
temp6<-NA
temp6[raw_dat$DD734=="Did not smoke"|raw_dat$DD740==0|raw_dat$DD1116==0]<-0
temp6[(raw_dat$DD740>0 & raw_dat$DD740<10)|(raw_dat$DD1116>0 & raw_dat$DD1116<10)]<-1
temp6[(raw_dat$DD740>9 & raw_dat$DD740<20)|(raw_dat$DD1116>9& raw_dat$DD1116<20)]<-2
temp6[raw_dat$DD740>=20|raw_dat$DD1116>=20]<-3
temp18<-NA
temp18[raw_dat$EE603=="Do not smoke"|raw_dat$EE604==0]<-0
temp18[raw_dat$EE604>0 & raw_dat$EE604<10]<-1
temp18[raw_dat$EE604>9 & raw_dat$EE604<20]<-2
temp18[raw_dat$EE604>=20]<-3

temp_df<-data.frame(temp3,temp6,temp18)
suppressWarnings(temp_df$temp_var_318<-apply(temp_df,1,max,na.rm=T))
temp_df$temp_var_318[is.infinite(temp_df$temp_var_318)]<-NA
dat$smoking_mother_postnatal_ordinal<-temp_df$temp_var_318

dat$smoking_mother_postnatal_ordinal[dat$smoking_mother_postnatal_ordinal==0]<-"None"
dat$smoking_mother_postnatal_ordinal[dat$smoking_mother_postnatal_ordinal==1]<-"Light"
dat$smoking_mother_postnatal_ordinal[dat$smoking_mother_postnatal_ordinal==2]<-"Moderate"
dat$smoking_mother_postnatal_ordinal[dat$smoking_mother_postnatal_ordinal==3]<-"Heavy"

dat$smoking_mother_postnatal_ordinal<-factor(dat$smoking_mother_postnatal_ordinal,levels=c("None","Light","Moderate","Heavy"),ordered=T)

rm(temp_df,temp3,temp6,temp18,duplicates)

#postnatal: any vs none up to age 2
dat$smoking_mother_postnatal_binary<-NA
dat$smoking_mother_postnatal_binary[dat$smoking_mother_postnatal_ordinal%in%c("Light","Moderate","Heavy")]<-1
dat$smoking_mother_postnatal_binary[dat$smoking_mother_postnatal_ordinal=="None"]<-0

#Pregnancy: Any smoking vs no smoking at any time during pregnancy
dat$smoking_mother_ever_pregnancy_binary<-NA
dat$smoking_mother_ever_pregnancy_binary[dat$smoking_mother_secondtrim_binary==0 &dat$smoking_mother_thirdtrim_binary==0] <- 0
dat$smoking_mother_ever_pregnancy_binary[dat$smoking_mother_secondtrim_binary==1 | dat$smoking_mother_thirdtrim_binary==1] <- 1


### Paternal

dat$smoking_father_ever_life_binary<-NA
dat$smoking_father_ever_life_binary[raw_dat$FF214=="Yes"]<-1
dat$smoking_father_ever_life_binary[raw_dat$FF214=="No"]<-0

#ever life any report
dat$smoking_father_ever_life_binary_anyreport <- dat$smoking_father_ever_life_binary

#smoking before age 11 vs no smoking before age 11
 ## NA

#preconception: number of cigarettes per day in the 6 months prior to pregnancy 
dat$smoking_father_preconception_ordinal<-NA
tmp_cigs_daily <- raw_dat$FF217
tmp_cigs_sometimes <- raw_dat$FF216/7 #divided by 7 because if it's sometimes, the question is n cigs pwk
tmp_cigs_pd <- apply(data.frame(tmp_cigs_sometimes,tmp_cigs_daily),1,max,na.rm = T)
tmp_cigs_pd[is.infinite(tmp_cigs_pd)]<-NA
dat$smoking_father_preconception_ordinal[tmp_cigs_pd==0|raw_dat$FF215=="No"]<-"None"
dat$smoking_father_preconception_ordinal[tmp_cigs_pd>0&tmp_cigs_pd<10]<-"Light" #i.e. <10 but not 0
dat$smoking_father_preconception_ordinal[tmp_cigs_pd<20 & tmp_cigs_pd>=10]<-"Moderate" # i.e. <20, >=10
dat$smoking_father_preconception_ordinal[tmp_cigs_pd>=20]<-"Heavy" #i.e. greater than 19
dat$smoking_father_preconception_ordinal<-factor(dat$smoking_father_preconception_ordinal,levels=c("None","Light","Moderate","Heavy"), ordered=T)

#pre-conception: any vs no smoking in the first 3 months prior to pregnancy (FF215 did you smoke sometimes/daily/no)
dat$smoking_father_preconception_binary<-NA
dat$smoking_father_preconception_binary[raw_dat$FF215=="Sometimes" | raw_dat$FF215=="Daily"|raw_dat$FF215=="Sometimes + Daily"| raw_dat$FF215=="No + Sometimes"|raw_dat$FF215=="No + Daily"|raw_dat$FF215=="No + Sometimes + Daily"|dat$smoking_father_preconception_ordinal%in% c("Light","Moderate","Heavy")]<-1
dat$smoking_father_preconception_binary[dat$smoking_father_preconception_ordinal=="None"]<-0

#preconception binary maternal report
## this is maternal report of dad smoking before and after became pregnant
dat$smoking_father_preconception_binary_mreport<-NA
dat$smoking_father_preconception_binary_mreport[raw_dat$AA1353=="No"]<-0
dat$smoking_father_preconception_binary_mreport[raw_dat$AA1353=="Yes"]<-1

#preconception any report
dat$smoking_father_preconception_binary_anyreport <- dat$smoking_father_preconception_binary
dat$smoking_father_preconception_binary_anyreport[is.na(dat$smoking_father_preconception_binary)]<- dat$smoking_father_preconception_binary_mreport[is.na(dat$smoking_father_preconception_binary)]

dat$smoking_father_preconception_ordinal_anyreport <- dat$smoking_father_preconception_ordinal
# any vs no smoking in the first trimester(<12 weeks)
## NA

#number of cigarettes per day in the first trimester
## NA

#number of cigarettes per day in the second trimester (15 wks)
dat$smoking_father_secondtrim_ordinal<-NA
tmp_cigs_daily <- raw_dat$FF220
tmp_cigs_sometimes <- raw_dat$FF219/7 #divided by 7 because if it's sometimes, the question is n cigs pwk
tmp_cigs_pd <- apply(data.frame(tmp_cigs_sometimes,tmp_cigs_daily),1,max,na.rm = T)
tmp_cigs_pd[is.infinite(tmp_cigs_pd)]<-NA
dat$smoking_father_secondtrim_ordinal[tmp_cigs_pd==0|raw_dat$FF218=="No"]<-"None"
dat$smoking_father_secondtrim_ordinal[tmp_cigs_pd>0&tmp_cigs_pd<10]<-"Light" #i.e. <10 but not 0
dat$smoking_father_secondtrim_ordinal[tmp_cigs_pd<20 & tmp_cigs_pd>=10]<-"Moderate" # i.e. <20, >=10
dat$smoking_father_secondtrim_ordinal[tmp_cigs_pd>=20]<-"Heavy" #i.e. greater than 19
dat$smoking_father_secondtrim_ordinal<-factor(dat$smoking_father_secondtrim_ordinal,levels=c("None","Light","Moderate","Heavy"), ordered=T)

# any vs no smoking in the second trimester
#(asked at 15 weeks)
dat$smoking_father_secondtrim_binary<-NA
dat$smoking_father_secondtrim_binary[raw_dat$FF218=="Sometimes" | raw_dat$FF218=="Daily"|raw_dat$FF218=="Sometimes + Daily"| raw_dat$FF218=="No + Sometimes"|raw_dat$FF218=="No + Daily"|raw_dat$FF218=="No + Sometimes + Daily"|dat$smoking_father_secondtrim_ordinal%in% c("Light","Moderate","Heavy")]<-1
dat$smoking_father_secondtrim_binary[dat$smoking_father_secondtrim_ordinal=="None"]<-0

dat$smoking_father_secondtrim_binary_mreport<-NA
dat$smoking_father_secondtrim_binary_mreport[raw_dat$AA1354=="Yes"]<-1
dat$smoking_father_secondtrim_binary_mreport[raw_dat$AA1354=="No"]<-0

#secondtrim any report
dat$smoking_father_secondtrim_binary_anyreport <- dat$smoking_father_secondtrim_binary
dat$smoking_father_secondtrim_binary_anyreport[is.na(dat$smoking_father_secondtrim_binary)]<- dat$smoking_father_secondtrim_binary_mreport[is.na(dat$smoking_father_secondtrim_binary)]
dat$smoking_father_secondtrim_ordinal_anyreport <- dat$smoking_father_secondtrim_ordinal

#number of cigarettes per day in the third trimester (27 wks)
dat$smoking_father_thirdtrim_ordinal_mreport<-NA
tmp_cigs_daily <- raw_dat$CC1042
tmp_cigs_sometimes <- raw_dat$CC1041/7 #divided by 7 because if it's sometimes, the question is n cigs pwk
tmp_cigs_pd <- apply(data.frame(tmp_cigs_sometimes,tmp_cigs_daily,raw_dat$DD741,raw_dat$DD1117),1,max,na.rm = T)
tmp_cigs_pd[is.infinite(tmp_cigs_pd)]<-NA
dat$smoking_father_thirdtrim_ordinal_mreport[tmp_cigs_pd==0|raw_dat$CC1040=="No"|raw_dat$DD735=="Did not smoke"]<-"None"
dat$smoking_father_thirdtrim_ordinal_mreport[tmp_cigs_pd>0&tmp_cigs_pd<10]<-"Light" #i.e. <10 but not 0
dat$smoking_father_thirdtrim_ordinal_mreport[tmp_cigs_pd<20 & tmp_cigs_pd>=10]<-"Moderate" # i.e. <20, >=10
dat$smoking_father_thirdtrim_ordinal_mreport[tmp_cigs_pd>=20]<-"Heavy" #i.e. greater than 19
dat$smoking_father_thirdtrim_ordinal_mreport<-factor(dat$smoking_father_thirdtrim_ordinal_mreport,levels=c("None","Light","Moderate","Heavy"), ordered=T)

# any vs no smoking in the third trimester
#(asked at 27 weeks)
dat$smoking_father_thirdtrim_binary_mreport<-NA
dat$smoking_father_thirdtrim_binary_mreport[raw_dat$DD735 %in% c("Smoked sometimes","Smoked every day")|raw_dat$CC1040=="Sometimes" | raw_dat$CC1040=="Daily"|raw_dat$CC1040=="Sometimes + Daily"| raw_dat$CC1040=="No + Sometimes"|raw_dat$CC1040=="No + Daily"|raw_dat$CC1040=="No + Sometimes + Daily"|dat$smoking_father_thirdtrim_ordinal_mreport%in% c("Light","Moderate","Heavy")]<-1
dat$smoking_father_thirdtrim_binary_mreport[dat$smoking_father_thirdtrim_ordinal_mreport=="None"]<-0

#thirdtrim any report
dat$smoking_father_thirdtrim_ordinal_anyreport <- dat$smoking_father_thirdtrim_ordinal_mreport
dat$smoking_father_thirdtrim_binary_anyreport <- dat$smoking_father_thirdtrim_binary_mreport

#ever in pregnancy
dat$smoking_father_ever_pregnancy_binary <- dat$smoking_father_secondtrim_binary

dat$smoking_father_ever_pregnancy_binary_mreport <- NA
dat$smoking_father_ever_pregnancy_binary_mreport[dat$smoking_father_secondtrim_binary_mreport==0&dat$smoking_father_thirdtrim_binary_mreport==0]<-0
dat$smoking_father_ever_pregnancy_binary_mreport[dat$smoking_father_secondtrim_binary_mreport==1|dat$smoking_father_thirdtrim_binary_mreport==1]<-1


dat$smoking_father_ever_pregnancy_binary_anyreport <- NA
dat$smoking_father_ever_pregnancy_binary_anyreport[dat$smoking_father_secondtrim_binary_anyreport==0&dat$smoking_father_thirdtrim_binary_anyreport==0]<-0
dat$smoking_father_ever_pregnancy_binary_anyreport[dat$smoking_father_secondtrim_binary_anyreport==1|dat$smoking_father_thirdtrim_binary_anyreport==1]<-1


# postnatal ordinal (first 1000 days)
temp3<-NA #0-3 months after birth
temp3[raw_dat$DD736=="Did not smoke"|raw_dat$DD742==0|raw_dat$DD1118==0]<-0 #ncigs pd if every day and if sometimes
temp3[which((raw_dat$DD742>0 & raw_dat$DD742<10)|(raw_dat$DD1118>0 & raw_dat$DD1118<10))]<-1
temp3[which((raw_dat$DD742>9 & raw_dat$DD742<20)|(raw_dat$DD1118>9& raw_dat$DD1118<20))]<-2
temp3[raw_dat$DD742>=20|raw_dat$DD1118>=20]<-3

temp6<-NA
temp6[raw_dat$DD737=="Did not smoke"|raw_dat$DD743==0|raw_dat$DD1119==0]<-0
temp6[which((raw_dat$DD743>0 & raw_dat$DD743<10)|(raw_dat$DD1119>0 & raw_dat$DD1119<10))]<-1
temp6[which((raw_dat$DD743>9 & raw_dat$DD743<20)|(raw_dat$DD1119>9& raw_dat$DD1119<20))]<-2
temp6[raw_dat$DD743>=20|raw_dat$DD1119>=20]<-3
temp18<-NA
temp18[raw_dat$EE605=="Do not smoke"|raw_dat$EE606==0]<-0
temp18[raw_dat$EE606>0 & raw_dat$EE606<10]<-1
temp18[raw_dat$EE606>9 & raw_dat$EE606<20]<-2
temp18[raw_dat$EE606>=20]<-3

temp_df<-data.frame(temp3,temp6,temp18)
suppressWarnings(temp_df$temp_var_318<-apply(temp_df,1,max,na.rm=T))
temp_df$temp_var_318[is.infinite(temp_df$temp_var_318)]<-NA
dat$smoking_father_postnatal_ordinal_mreport<-temp_df$temp_var_318

dat$smoking_father_postnatal_ordinal_mreport[dat$smoking_father_postnatal_ordinal_mreport==0]<-"None"
dat$smoking_father_postnatal_ordinal_mreport[dat$smoking_father_postnatal_ordinal_mreport==1]<-"Light"
dat$smoking_father_postnatal_ordinal_mreport[dat$smoking_father_postnatal_ordinal_mreport==2]<-"Moderate"
dat$smoking_father_postnatal_ordinal_mreport[dat$smoking_father_postnatal_ordinal_mreport==3]<-"Heavy"

dat$smoking_father_postnatal_ordinal_mreport<-factor(dat$smoking_father_postnatal_ordinal_mreport,levels=c("None","Light","Moderate","Heavy"),ordered=T)

rm(temp_df,temp3,temp6,temp18)

#postnatal: any vs none up to age 2
dat$smoking_father_postnatal_binary_mreport<-NA
dat$smoking_father_postnatal_binary_mreport[dat$smoking_father_postnatal_ordinal_mreport%in%c("Light","Moderate","Heavy")]<-1
dat$smoking_father_postnatal_binary_mreport[dat$smoking_father_postnatal_ordinal_mreport=="None"]<-0

# anyreport
dat$smoking_father_postnatal_ordinal_anyreport <- dat$smoking_father_postnatal_ordinal_mreport
dat$smoking_father_postnatal_binary_anyreport <- dat$smoking_father_postnatal_binary_mreport

############################################
#######           Alcohol            #######
############################################

### Maternal

# Pre-conception: number of units per day in the 3 months prior to pregnancy (ordered categorical)
  ## used AA1453 ("How often did you consume alcohol in the 3 months before you became pregnant?" rather than AA1464 ("How many units of alcohol do you usually drink when you consume alcohol?" )
## because AA1464 has no indication of the frequency of drinks (e.g. they might have drank 10 drinks but only once and be classed as heavy, or they might drink 1 glass a day and be classed as light)
## AA1453 has options that are more in line with those for ALSPAC and BiB
## updated to also inclde CC1156 ("How often did you consume alcohol before and how often do you consume it now?" 3 months before last period)
dat$alcohol_mother_preconception_ordinal<-NA
dat$alcohol_mother_preconception_ordinal[raw_dat$AA1453=="Never"|raw_dat$CC1156=="(7) Never"]<-"None"
dat$alcohol_mother_preconception_ordinal[raw_dat$AA1453%in%c("Less than once per month","Approximately 1-3 times a month","Approximately once a week")]<-"Light" #<1glass pwk
dat$alcohol_mother_preconception_ordinal[raw_dat$CC1156%in%c("Less than once per month","Approximately 1-3 times a month","Approximately once a week")]<-"Light" #<1glass pwk
dat$alcohol_mother_preconception_ordinal[raw_dat$AA1453%in%c("Approximately 2-3 times a week","Approximately 4-5 times a week")]<-"Moderate" # 1+pwk, not daily
dat$alcohol_mother_preconception_ordinal[raw_dat$CC1156%in%c("Approximately 2-3 times a week","Approximately 4-5 times a week")]<-"Moderate" # 1+pwk, not daily
dat$alcohol_mother_preconception_ordinal[raw_dat$AA1453=="Approximately 6-7 times a week"]<-"Heavy" #daily
dat$alcohol_mother_preconception_ordinal[raw_dat$CC1156=="Approximately 6-7 times a week"]<-"Heavy" #daily
dat$alcohol_mother_preconception_ordinal<-factor(dat$alcohol_mother_preconception_ordinal,levels=c("None","Light","Moderate","Heavy"),ordered=T)

# Pre-conception: Any vs no drinking in the 3 months prior to pregnancy

dat$alcohol_mother_preconception_binary<-NA
dat$alcohol_mother_preconception_binary[dat$alcohol_mother_preconception_ordinal=="None"]<-0
dat$alcohol_mother_preconception_binary[dat$alcohol_mother_preconception_ordinal%in%c("Light","Moderate","Heavy")]<-1

# First trimester ordinal (ordered categorical)
## CC1157 ("How often did you consume alcohol before and how often do you consume it now" In this pregnancy, week 0-12)
dat$alcohol_mother_firsttrim_ordinal<-NA
dat$alcohol_mother_firsttrim_ordinal[raw_dat$CC1157=="(7) Never"]<-"None"
dat$alcohol_mother_firsttrim_ordinal[raw_dat$CC1157%in%c("Less than once per month","Approximately 1-3 times a month","Approximately once a week")]<-"Light" #<1glass pwk
dat$alcohol_mother_firsttrim_ordinal[raw_dat$CC1157%in%c("Approximately 2-3 times a week","Approximately 4-5 times a week")]<-"Moderate" # 1+pwk, not daily
dat$alcohol_mother_firsttrim_ordinal[raw_dat$CC1157=="Approximately 6-7 times a week"]<-"Heavy" #daily
dat$alcohol_mother_firsttrim_ordinal<-factor(dat$alcohol_mother_firsttrim_ordinal,levels=c("None","Light","Moderate","Heavy"),ordered=T)

# First trimester binary
dat$alcohol_mother_firsttrim_binary<-NA
dat$alcohol_mother_firsttrim_binary[dat$alcohol_mother_firsttrim_ordinal=="None"]<-0
dat$alcohol_mother_firsttrim_binary[dat$alcohol_mother_firsttrim_ordinal%in%c("Light","Moderate","Heavy")]<-1

# Second trimester ordinal (ordered categorical)
## CC1158 ("How often did you consume alcohol before and how often do you consume it now" In this pregnancy, week 13-24)
## Taken out AA1454 because although it was (I think?) asked in the second trimester, the mother could have been responding about first or second trimester.
dat$alcohol_mother_secondtrim_ordinal<-NA
dat$alcohol_mother_secondtrim_ordinal[raw_dat$CC1158=="(7) Never"]<-"None"
dat$alcohol_mother_secondtrim_ordinal[raw_dat$CC1158%in%c("Less than once per month","Approximately 1-3 times a month","Approximately once a week")]<-"Light" #<1glass pwk
dat$alcohol_mother_secondtrim_ordinal[raw_dat$CC1158%in%c("Approximately 2-3 times a week","Approximately 4-5 times a week")]<-"Moderate" # 1+pwk, not daily
dat$alcohol_mother_secondtrim_ordinal[raw_dat$CC1158=="Approximately 6-7 times a week"]<-"Heavy" #daily
dat$alcohol_mother_secondtrim_ordinal<-factor(dat$alcohol_mother_secondtrim_ordinal,levels=c("None","Light","Moderate","Heavy"),ordered=T)

# Second trimester binary
dat$alcohol_mother_secondtrim_binary<-NA
dat$alcohol_mother_secondtrim_binary[dat$alcohol_mother_secondtrim_ordinal=="None"]<-0
dat$alcohol_mother_secondtrim_binary[dat$alcohol_mother_secondtrim_ordinal%in%c("Light","Moderate","Heavy")]<-1

# Third trimester ordinal
## CC1159 ("How often did you consume alcohol before and how often do you consume it now" In this pregnancy, week 0-12)
dat$alcohol_mother_thirdtrim_ordinal<-NA
dat$alcohol_mother_thirdtrim_ordinal[raw_dat$CC1159=="(7) Never"]<-"None"
dat$alcohol_mother_thirdtrim_ordinal[raw_dat$CC1159%in%c("Less than once per month","Approximately 1-3 times a month","Approximately once a week")]<-"Light" #<1glass pwk
dat$alcohol_mother_thirdtrim_ordinal[raw_dat$CC1159%in%c("Approximately 2-3 times a week","Approximately 4-5 times a week")]<-"Moderate" # 1+pwk, not daily
dat$alcohol_mother_thirdtrim_ordinal[raw_dat$CC1159=="Approximately 6-7 times a week"]<-"Heavy" #daily
dat$alcohol_mother_thirdtrim_ordinal<-factor(dat$alcohol_mother_thirdtrim_ordinal,levels=c("None","Light","Moderate","Heavy"),ordered=T)

# third trimester binary
dat$alcohol_mother_thirdtrim_binary<-NA
dat$alcohol_mother_thirdtrim_binary[dat$alcohol_mother_thirdtrim_ordinal=="None"]<-0
dat$alcohol_mother_thirdtrim_binary[dat$alcohol_mother_thirdtrim_ordinal%in%c("Light","Moderate","Heavy")]<-1

# Pregnancy: any vs no drinking at any time during pregnancy
dat$alcohol_mother_ever_pregnancy_binary<-NA
dat$alcohol_mother_ever_pregnancy_binary[dat$alcohol_mother_firsttrim_binary==0&dat$alcohol_mother_secondtrim_binary==0& dat$alcohol_mother_thirdtrim_binary==0]<-0
dat$alcohol_mother_ever_pregnancy_binary[dat$alcohol_mother_firsttrim_binary==1|dat$alcohol_mother_secondtrim_binary==1| dat$alcohol_mother_thirdtrim_binary==1]<-1

# binge drinking in the first trimester (binary)
## binge measures are asked as 5 or more units or more at least once 
# binge drinking 3 months preconception
dat$alcohol_mother_binge_preconception_binary<-NA
dat$alcohol_mother_binge_preconception_binary[raw_dat$CC1160=="Never"]<-0
dat$alcohol_mother_binge_preconception_binary[raw_dat$CC1160=="1-3 times per month"|raw_dat$CC1160=="Less than once a month"|raw_dat$CC1160=="Once per week"|raw_dat$CC1160=="Several times per week"]<-1

# binge drinking in the first trimester (binary)
dat$alcohol_mother_binge1_binary<-NA
dat$alcohol_mother_binge1_binary[raw_dat$CC1161=="Never"]<-0
dat$alcohol_mother_binge1_binary[raw_dat$CC1161=="1-3 times per month"|raw_dat$CC1161=="Less than once a month"|raw_dat$CC1161=="Once per week"|raw_dat$CC1161=="Several times per week"]<-1

# binge drinking in the second trimester (binary)
dat$alcohol_mother_binge2_binary<-NA
dat$alcohol_mother_binge2_binary[raw_dat$CC1162=="Never"]<-0
dat$alcohol_mother_binge2_binary[raw_dat$CC1162=="1-3 times per month"|raw_dat$CC1162=="Less than once a month"|raw_dat$CC1162=="Once per week"|raw_dat$CC1162=="Several times per week"]<-1

# binge drinking in the third trimester (binary)
dat$alcohol_mother_binge3_binary<-NA
dat$alcohol_mother_binge3_binary[raw_dat$CC1163=="Never"]<-0
dat$alcohol_mother_binge3_binary[raw_dat$CC1163=="1-3 times per month"|raw_dat$CC1163=="Less than once a month"|raw_dat$CC1163=="Once per week"|raw_dat$CC1163=="Several times per week"]<-1

# binge drinking at any point during pregnancy
dat$alcohol_mother_bingepreg_binary<-NA
dat$alcohol_mother_bingepreg_binary[dat$alcohol_mother_binge1_binary==0 & dat$alcohol_mother_binge2_binary==0&dat$alcohol_mother_binge3_binary==0]<-0
dat$alcohol_mother_bingepreg_binary[dat$alcohol_mother_binge1_binary==1 | dat$alcohol_mother_binge2_binary==1|dat$alcohol_mother_binge3_binary==1]<-1

# postnatal alcohol use ordinal, up to 2 years 21 months (first 1000 days)
temp3<-NA
temp3[raw_dat$DD775=="(7) Never"]<-0
temp3[raw_dat$DD775%in% c("Less than once per month","Approximately 1-3 times a month","Approximately once a week")]<-1
temp3[raw_dat$DD775%in%c("Approximately 2-3 times a week","Approximately 4-5 times a week")]<-2
temp3[raw_dat$DD775=="Approximately 6-7 times a week"]<-3

temp6<-NA
temp6[raw_dat$DD776=="(7) Never"]<-0
temp6[raw_dat$DD776%in% c("Less than once per month","Approximately 1-3 times a month","Approximately once a week")]<-1
temp6[raw_dat$DD776%in%c("Approximately 2-3 times a week","Approximately 4-5 times a week")]<-2
temp6[raw_dat$DD776=="Approximately 6-7 times a week"]<-3

temp18<-NA
temp18[raw_dat$EE607=="Never"]<-0
temp18[raw_dat$EE607%in% c("Less than once per month","Approximately 1-3 times a month","Approximately once a week")]<-1
temp18[raw_dat$EE607%in%c("Approximately 2-3 times a week","Approximately 4-5 times a week")]<-2
temp18[raw_dat$EE607=="Approximately 6-7 times a week"]<-3


temp_df<-data.frame(temp3,temp6,temp18)
suppressWarnings(temp_df$temp_var_test<-apply(temp_df,1,max,na.rm=T))
temp_df$temp_var_test[is.infinite(temp_df$temp_var_test)]<-NA
dat$alcohol_mother_postnatal_ordinal<-temp_df$temp_var_test

dat$alcohol_mother_postnatal_ordinal[dat$alcohol_mother_postnatal_ordinal==0]<-"None"
dat$alcohol_mother_postnatal_ordinal[dat$alcohol_mother_postnatal_ordinal==1]<-"Light"
dat$alcohol_mother_postnatal_ordinal[dat$alcohol_mother_postnatal_ordinal==2]<-"Moderate"
dat$alcohol_mother_postnatal_ordinal[dat$alcohol_mother_postnatal_ordinal==3]<-"Heavy"

dat$alcohol_mother_postnatal_ordinal<-factor(dat$alcohol_mother_postnatal_ordinal,levels=c("None","Light","Moderate","Heavy"),ordered=T)

rm(temp_df,temp3,temp6,temp18)

dat$alcohol_mother_postnatal_binary <- NA
dat$alcohol_mother_postnatal_binary[dat$alcohol_mother_postnatal_ordinal=="None"]<-0
dat$alcohol_mother_postnatal_binary[dat$alcohol_mother_postnatal_ordinal%in% c("Light","Moderate","Heavy")]<-1

### CHECKED UP TO HERE 4th JANUARY ####

## Paternal
# Pre-conception: number of units per day in the 3 months prior to pregnancy (ordered categorical)
## used FF243 ("How often did you drink alcohol in the 6 months before your partner became pregnant?"
dat$alcohol_father_preconception_ordinal<-NA
dat$alcohol_father_preconception_ordinal[raw_dat$FF243=="(7) Never"]<-"None"
dat$alcohol_father_preconception_ordinal[raw_dat$FF243%in%c("Less than once per month","Approximately 1-3 times a month","Approximately once a week")]<-"Light" #<1glass pwk
dat$alcohol_father_preconception_ordinal[raw_dat$FF243%in%c("Approximately 2-3 times a week","Approximately 4-5 times a week")]<-"Moderate" # 1+pwk, not daily
dat$alcohol_father_preconception_ordinal[raw_dat$FF243=="Approximately 6-7 times a week"]<-"Heavy" #daily
dat$alcohol_father_preconception_ordinal<-factor(dat$alcohol_father_preconception_ordinal,levels=c("None","Light","Moderate","Heavy"),ordered=T)

dat$alcohol_father_preconception_ordinal_anyreport <- dat$alcohol_father_preconception_ordinal
# Pre-conception: Any vs no drinking in the 3 months prior to pregnancy

dat$alcohol_father_preconception_binary<-NA
dat$alcohol_father_preconception_binary[dat$alcohol_father_preconception_ordinal=="None"]<-0
dat$alcohol_father_preconception_binary[dat$alcohol_father_preconception_ordinal%in%c("Light","Moderate","Heavy")]<-1
dat$alcohol_father_preconception_binary_anyreport <-dat$alcohol_father_preconception_binary

# Second trimester ordinal (ordered categorical)
## FF244 ("How often did you consume alcohol before and how often do you consume it now that your partner is pregnant" Now)
## Note: they could have been responding about first or second trimester.
dat$alcohol_father_secondtrim_ordinal<-NA
dat$alcohol_father_secondtrim_ordinal[raw_dat$FF244=="(7) Never"]<-"None"
dat$alcohol_father_secondtrim_ordinal[raw_dat$FF244%in%c("Less than once per month","Approximately 1-3 times a month","Approximately once a week")]<-"Light" #<1glass pwk
dat$alcohol_father_secondtrim_ordinal[raw_dat$FF244%in%c("Approximately 2-3 times a week","Approximately 4-5 times a week")]<-"Moderate" # 1+pwk, not daily
dat$alcohol_father_secondtrim_ordinal[raw_dat$FF244=="Approximately 6-7 times a week"]<-"Heavy" #daily
dat$alcohol_father_secondtrim_ordinal<-factor(dat$alcohol_father_secondtrim_ordinal,levels=c("None","Light","Moderate","Heavy"),ordered=T)
dat$alcohol_father_secondtrim_ordinal_anyreport <- dat$alcohol_father_secondtrim_ordinal

# Second trimester binary
dat$alcohol_father_secondtrim_binary<-NA
dat$alcohol_father_secondtrim_binary[dat$alcohol_father_secondtrim_ordinal=="None"]<-0
dat$alcohol_father_secondtrim_binary[dat$alcohol_father_secondtrim_ordinal%in%c("Light","Moderate","Heavy")]<-1
dat$alcohol_father_secondtrim_binary_anyreport <- dat$alcohol_father_secondtrim_binary

# Pregnancy: any vs no drinking at any time during pregnancy
# this will be the same as second trimester ever drinking as only timepoint we have
dat$alcohol_father_ever_pregnancy_binary<-dat$alcohol_father_secondtrim_binary
dat$alcohol_father_ever_pregnancy_ordinal<-dat$alcohol_father_secondtrim_ordinal
dat$alcohol_father_ever_pregnancy_binary_anyreport <-dat$alcohol_father_ever_pregnancy_binary
dat$alcohol_father_ever_pregnancy_ordinal_anyreport <-dat$alcohol_father_ever_pregnancy_ordinal

# binge drinking 6 months preconception (5 alcohol units or more on one occasion)
dat$alcohol_father_binge_preconception_binary<-NA
dat$alcohol_father_binge_preconception_binary[raw_dat$FF473=="Never"]<-0
dat$alcohol_father_binge_preconception_binary[raw_dat$FF473=="1-3 times per month"|raw_dat$FF473=="Less than once a month"|raw_dat$FF473=="Once per week"|raw_dat$FF473=="Several times per week"]<-1
dat$alcohol_father_binge_preconception_binary_anyreport <- dat$alcohol_father_binge_preconception_binary
# binge drinking in the first trimester (binary)
## binge measures are asked as 5 or more units or more at least once 
  #NA

# binge drinking in the second trimester (binary)
dat$alcohol_father_binge2_binary<-NA
dat$alcohol_father_binge2_binary[raw_dat$FF474=="Never"]<-0
dat$alcohol_father_binge2_binary[raw_dat$FF474=="1-3 times per month"|raw_dat$FF474=="Less than once a month"|raw_dat$FF474=="Once per week"|raw_dat$FF474=="Several times per week"]<-1
dat$alcohol_father_binge2_binary_anyreport <- dat$alcohol_father_binge2_binary
# binge drinking in the third trimester (binary)
  #NA

# binge drinking at any point during pregnancy
  #THIS WOULD BE THE SAME AS 2ND TRIMESTER VARIABLE
dat$alcohol_father_bingepreg_binary<-dat$alcohol_father_binge2_binary
dat$alcohol_father_bingepreg_binary_anyreport <- dat$alcohol_father_bingepreg_binary

# postnatal (kayleigh had used FF467 and FF468, but this is a bit odd because they are not postnatal - they're during pregnancy)

############################################
#######           Caffeine           #######
############################################


# Ideal harmonised caffeine exposures for both parents are:

# Pre-conception: Any vs no caffeine in the 3 months prior to pregnancy
# Pre-conception: Amt of caffeine (mg/day) per day int he 3 months prior to pregnancy
# Pregnancy: Caffeine from coffee first trimester (mg/day)
# Pregnancy: Caffeine from tea first trimester (mg/day)
# Pregnancy: Caffeine from cola first trimester (mg/day)
# Pregnancy: Total caffeine in first trimester (mg/day)
# Pregnancy: Caffeine from coffee second trimester (mg/day)
# Pregnancy: Caffeine from tea second trimester (mg/day)
# Pregnancy: Caffeine from cola second trimester (mg/day)
# Pregnancy: Total caffeine in second trimester (mg/day)
# Pregnancy: Caffeine from coffee third trimester (mg/day)
# Pregnancy: Caffeine from tea third trimester (mg/day)
# Pregnancy: Caffeine from cola third trimester (mg/day)
# Pregnancy: Total caffeine in third trimester (mg/day)
# Pregnancy: Any caffeine vs no caffeine in the first trimester (mg/day)
# Pregnancy: Any caffeine vs no caffeine in the second trimester (mg/day)
# Pregnancy: Any caffeine vs no caffeine in the third trimester (mg/day)
# Pregnancy: Average caffeine per day in pregnancy
# Pregnancy: any caffeine vs no caffeine in pregnancy
# Postnatal: Caffeine from tea up to age 2 (mg/day)
# Postnatal: Caffeine from cola up to age 2 (mg/day)
# Postnatal: Total caffeine up to age 2 (mg/day)
# Postnatal: Any vs none


### Maternal

#  Amt of caffeine ((cups) per day before pregnancy

calculate_caffeine <- function(variablename,decaf_variablename){
  var <- raw_dat[,variablename]
  var[var=="Consumption have been reported by a mark but no amount given"]<-1 #give 1 cup if amount not given
  var<-as.numeric(as.character(var))
  var[decaf_variablename==1]<-0
  var
}

caffeine_variables <- list(filtercoffee=c("AA1377","AA1378","AA1379"),
                           instantcoffee=c("AA1380","AA1381","AA1382"),
                           boiledcoffee=c("AA1383","AA1384","AA1385"),
                           tea=c("AA1386","AA1387","AA1388"),
                           cola=c("AA1392","AA1393","AA1394"),
                           dietcola=c("AA1398","AA1399","AA1400"))

before <- lapply(caffeine_variables,function(x) calculate_caffeine(variablename=x[1],decaf_variablename=x[3]))
now <- lapply(caffeine_variables,function(x) calculate_caffeine(variablename=x[2],decaf_variablename=x[3]))

#Preconception

dat$caffeine_mother_tea_preconception_continuous <- before$tea * 27 #mg/day
dat$caffeine_mother_coffee_preconception_continuous <- rowSums(data.frame(before$filtercoffee,before$instantcoffee,before$boiledcoffee),na.rm=T) * 57 #mg/day
dat$caffeine_mother_coffee_preconception_continuous[apply(data.frame(before$filtercoffee,before$instantcoffee,before$boiledcoffee),1,function(x)all(is.na(x)))]<-NA
dat$caffeine_mother_cola_preconception_continuous <- rowSums(data.frame(before$cola,before$dietcola),na.rm=T) * 20 #mg/day
dat$caffeine_mother_cola_preconception_continuous[apply(data.frame(before$cola,before$dietcola),1,function(x)all(is.na(x)))]<-NA
dat$caffeine_mother_total_preconception_continuous <- rowSums(data.frame(dat$caffeine_mother_coffee_preconception_continuous,
                                                              dat$caffeine_mother_tea_preconception_continuous,
                                                              dat$caffeine_mother_cola_preconception_continuous),na.rm=T)
dat$caffeine_mother_total_preconception_continuous[apply(data.frame(dat$caffeine_mother_coffee_preconception_continuous,
                                                                         dat$caffeine_mother_tea_preconception_continuous,
                                                                         dat$caffeine_mother_cola_preconception_continuous),1,function(x)all(is.na(x)))]<-NA

dat$caffeine_mother_total_preconception_binary <-NA
dat$caffeine_mother_total_preconception_binary[dat$caffeine_mother_total_preconception_continuous==0]<-0
dat$caffeine_mother_total_preconception_binary[dat$caffeine_mother_total_preconception_continuous>0]<-1

dat$caffeine_mother_total_preconception_ordinal <- cut(dat$caffeine_mother_total_preconception_continuous,breaks = c(-1,0,200,400,Inf),labels = c("None","Light","Moderate","Heavy"),ordered_result = T)

# in second trimester

dat$caffeine_mother_tea_secondtrim_continuous <- now$tea * 27 #mg/day
dat$caffeine_mother_coffee_secondtrim_continuous <- rowSums(data.frame(now$filtercoffee,now$instantcoffee,now$boiledcoffee),na.rm=T) * 57 #mg/day
dat$caffeine_mother_coffee_secondtrim_continuous[apply(data.frame(now$filtercoffee,now$instantcoffee,now$boiledcoffee),1,function(x)all(is.na(x)))]<-NA
dat$caffeine_mother_cola_secondtrim_continuous <- rowSums(data.frame(now$cola,now$dietcola),na.rm=T) * 20 #mg/day
dat$caffeine_mother_cola_secondtrim_continuous[apply(data.frame(now$cola,now$dietcola),1,function(x)all(is.na(x)))]<-NA
dat$caffeine_mother_total_secondtrim_continuous <- rowSums(data.frame(dat$caffeine_mother_coffee_secondtrim_continuous,
                                                                         dat$caffeine_mother_tea_secondtrim_continuous,
                                                                         dat$caffeine_mother_cola_secondtrim_continuous),na.rm=T)
dat$caffeine_mother_total_secondtrim_continuous[apply(data.frame(dat$caffeine_mother_coffee_secondtrim_continuous,
                                                                    dat$caffeine_mother_tea_secondtrim_continuous,
                                                                    dat$caffeine_mother_cola_secondtrim_continuous),1,function(x)all(is.na(x)))]<-NA

dat$caffeine_mother_total_secondtrim_binary <-NA
dat$caffeine_mother_total_secondtrim_binary[dat$caffeine_mother_total_secondtrim_continuous==0]<-0
dat$caffeine_mother_total_secondtrim_binary[dat$caffeine_mother_total_secondtrim_continuous>0]<-1

dat$caffeine_mother_total_secondtrim_ordinal <- cut(dat$caffeine_mother_total_secondtrim_continuous,breaks = c(-1,0,200,400,Inf),labels = c("None","Light","Moderate","Heavy"),ordered_result = T)


# ever pregnancy
dat$caffeine_mother_total_ever_pregnancy_binary <- dat$caffeine_mother_total_secondtrim_binary
dat$caffeine_mother_total_ever_pregnancy_ordinal <- dat$caffeine_mother_total_secondtrim_ordinal
dat$caffeine_mother_total_ever_pregnancy_continuous <- dat$caffeine_mother_total_secondtrim_continuous


### Paternal


# filter and instant coffee
calculate_caffeine <- function(variablename){
var <- rep(NA,nrow(raw_dat))
original=raw_dat[,variablename]
var[original=="Seldom/never"]<-0
var[original=="1-6 glasses per week"]<-0.43 #(3 cups/7 days=0.43pd)
var[original=="1 glass per day"]<-1
var[original=="2-3 glasses per day"]<-2.5
var[original=="4 glasses or more a day"]<-4
var
}

caffeine_variables <-c(filterinstant="FF424",
                       boiledcafeteria="FF425",
                       othercoffee="FF426",
                       tea="FF427",
                       cola="FF420",
                       sugarfreecola="FF421")

caffeine_variables <- lapply(caffeine_variables,calculate_caffeine)


caffeine_variables <- lapply(1:length(caffeine_variables),
                             function(x){
                               caffeine_variables[[x]] * c(57,57,57,27,20,20)[x]
                             })

dat$caffeine_father_coffee_secondtrim_continuous <- rowSums(data.frame(caffeine_variables[1:3]),na.rm=T)
dat$caffeine_father_coffee_secondtrim_continuous[apply(data.frame(caffeine_variables[1:3]),1,function(x)all(is.na(x)))]<-NA

dat$caffeine_father_tea_secondtrim_continuous <- unlist(caffeine_variables[4])

dat$caffeine_father_cola_secondtrim_continuous <- rowSums(data.frame(caffeine_variables[5:6]),na.rm=T)
dat$caffeine_father_cola_secondtrim_continuous[apply(data.frame(caffeine_variables[5:6]),1,function(x)all(is.na(x)))]<-NA

dat$caffeine_father_total_secondtrim_continuous<-rowSums(data.frame(
  dat$caffeine_father_coffee_secondtrim_continuous,
  dat$caffeine_father_tea_secondtrim_continuous,
  dat$caffeine_father_cola_secondtrim_continuous),na.rm=T)

dat$caffeine_father_total_secondtrim_continuous[apply(data.frame(dat$caffeine_father_coffee_secondtrim_continuous,
                                                                 dat$caffeine_father_tea_secondtrim_continuous,
                                                                 dat$caffeine_father_cola_secondtrim_continuous),1,function(x)all(is.na(x)))]<-NA

dat$caffeine_father_total_secondtrim_binary <-NA
dat$caffeine_father_total_secondtrim_binary[dat$caffeine_father_total_secondtrim_continuous==0]<-0
dat$caffeine_father_total_secondtrim_binary[dat$caffeine_father_total_secondtrim_continuous>0]<-1

dat$caffeine_father_total_secondtrim_ordinal <- cut(dat$caffeine_father_total_secondtrim_continuous,breaks = c(-1,0,200,400,Inf),labels = c("None","Light","Moderate","Heavy"),ordered_result = T)
dat$caffeine_father_total_secondtrim_ordinal[is.na(dat$caffeine_father_total_secondtrim_continuous)]<-NA


dat$caffeine_father_total_ever_pregnancy_continuous <- dat$caffeine_father_total_secondtrim_continuous
dat$caffeine_father_total_ever_pregnancy_ordinal <- dat$caffeine_father_total_secondtrim_ordinal
dat$caffeine_father_total_ever_pregnancy_binary <- dat$caffeine_father_total_secondtrim_binary

caffeine_father_vars <- colnames(dat)[grep(colnames(dat),pattern="caffeine_father")]
for(var in caffeine_father_vars){
  dat[,paste0(var,"_anyreport")]<-dat[,var]
}

#############################################
#######     Physical activity         #######
#############################################

# 
# #Physical activity in the second trimester
# raw_dat$temp_physact_mother_secondtrim_ordinal_leisure<-NA
# raw_dat$temp_physact_mother_secondtrim_ordinal_leisure[raw_dat$AA1525=="Never"]<-0 # Never
# raw_dat$temp_physact_mother_secondtrim_ordinal_leisure[raw_dat$AA1525=="Less than once a week"|raw_dat$AA1525=="Once a week"|raw_dat$AA1525=="Twice a week"]<-1 # Never
# raw_dat$temp_physact_mother_secondtrim_ordinal_leisure[raw_dat$AA1525=="3-4 times a week"|raw_dat$AA1525=="5 times a week or more"]<-2
# 
# raw_dat$temp_physact_mother_secondtrim_ordinal_work<-NA
# raw_dat$temp_physact_mother_secondtrim_ordinal_work[raw_dat$AA1526=="Never"]<-0 # Never
# raw_dat$temp_physact_mother_secondtrim_ordinal_work[raw_dat$AA1526=="Less than once a week"|raw_dat$AA1526=="Once a week"|raw_dat$AA1526=="Twice a week"]<-1
# raw_dat$temp_physact_mother_secondtrim_ordinal_work[raw_dat$AA1526=="3-4 times a week"|raw_dat$AA1526=="5 times a week or more"]<-2 
# 
# dat$physact_mother_secondtrim_ordinal<-NA
# dat$physact_mother_secondtrim_ordinal[raw_dat$temp_physact_mother_secondtrim_ordinal_work==0 & raw_dat$temp_physact_mother_secondtrim_ordinal_leisure==0]<-"None"
# dat$physact_mother_secondtrim_ordinal[raw_dat$temp_physact_mother_secondtrim_ordinal_work==1 | raw_dat$temp_physact_mother_secondtrim_ordinal_leisure==1]<-"Light"
# dat$physact_mother_secondtrim_ordinal[raw_dat$temp_physact_mother_secondtrim_ordinal_work==2 | raw_dat$temp_physact_mother_secondtrim_ordinal_leisure==2]<-"Moderate"
# 
# dat$physact_mother_secondtrim_ordinal[dat$physact_mother_secondtrim_ordinal==0]<-"None"
# dat$physact_mother_secondtrim_ordinal[dat$physact_mother_secondtrim_ordinal==1]<-"Light"
# dat$physact_mother_secondtrim_ordinal[dat$physact_mother_secondtrim_ordinal==2]<-"Moderate"
# dat$physact_mother_secondtrim_ordinal<-factor(dat$physact_mother_secondtrim_ordinal,levels=c("None","Light","Moderate"),ordered=T)
# 
# #Physical activity in the third trimester
# raw_dat$temp_physact_mother_thirdtrim_ordinal_leisure<-NA
# raw_dat$temp_physact_mother_thirdtrim_ordinal_leisure[raw_dat$CC1001=="Never"]<-0 # Never
# raw_dat$temp_physact_mother_thirdtrim_ordinal_leisure[raw_dat$CC1001=="Less than once a week"|raw_dat$CC1001=="Once a week"|raw_dat$CC1001=="2 times a week"]<-1 # light
# raw_dat$temp_physact_mother_thirdtrim_ordinal_leisure[raw_dat$CC1001=="3-4 times a week"|raw_dat$CC1001=="5 times a week or more"]<-2 #Moderate
# 
# temp_physact_mother_thirdtrim_ordinal_work<-NA
# temp_physact_mother_thirdtrim_ordinal_work[raw_dat$CC1002=="Never"]<-0 # Never
# temp_physact_mother_thirdtrim_ordinal_work[raw_dat$CC1002=="Less than once a week"|raw_dat$CC1002=="Once a week"|raw_dat$CC1002=="2 times a week"]<-1 # Light
# temp_physact_mother_thirdtrim_ordinal_work[raw_dat$CC1002=="3-4 times a week"|raw_dat$CC1002=="5 times a week or more"]<-2 #Moderate
# 
# dat$physact_mother_thirdtrim_ordinal<-NA
# dat$physact_mother_thirdtrim_ordinal[raw_dat$temp_physact_mother_thirdtrim_ordinal_work==0 & raw_dat$temp_physact_mother_thirdtrim_ordinal_leisure==0]<-"None"
# dat$physact_mother_thirdtrim_ordinal[raw_dat$temp_physact_mother_thirdtrim_ordinal_work==1 | raw_dat$temp_physact_mother_thirdtrim_ordinal_leisure==1]<-"Light"
# dat$physact_mother_thirdtrim_ordinal[raw_dat$temp_physact_mother_thirdtrim_ordinal_work==2 | raw_dat$temp_physact_mother_thirdtrim_ordinal_leisure==2]<-"Moderate"
# dat$physact_mother_thirdtrim_ordinal<-factor(dat$physact_mother_thirdtrim_ordinal,levels=c("None","Light","Moderate"),ordered=T)
# 
# 
# #Physical activity at any time in pregnancy
# dat$physact_mother_ever_pregnancy<-NA
# dat$physact_mother_ever_pregnancy[dat$physact_mother_secondtrim_ordinal=="None"& dat$physact_mother_thirdtrim_ordinal=="None"]<-0
# dat$physact_mother_ever_pregnancy[dat$physact_mother_secondtrim_ordinal=="Light"|dat$physact_mother_thirdtrim_ordinal=="Light"| dat$physact_mother_secondtrim_ordinal=="Moderate"|dat$physact_mother_thirdtrim_ordinal=="Moderate"]<-1
# 
# #Physical activity postnatally up to age 2
# raw_dat$temp_physact_mother_postnatal_ordinal_leisure<-NA
# raw_dat$temp_physact_mother_postnatal_ordinal_leisure[raw_dat$DD730=="Never"]<-0 # Never
# raw_dat$temp_physact_mother_postnatal_ordinal_leisure[raw_dat$DD730=="Less than once a week"|raw_dat$DD730=="Once a week"|raw_dat$DD730=="2 times a week"]<-1 
# raw_dat$temp_physact_mother_postnatal_ordinal_leisure[raw_dat$DD730=="3-4 times a week"|raw_dat$DD730=="5 times a week or more"]<-2
# 
# raw_dat$temp_physact_mother_postnatal_ordinal_work<-NA
# raw_dat$temp_physact_mother_postnatal_ordinal_work[raw_dat$DD731=="Never"]<-0 # Never
# raw_dat$temp_physact_mother_postnatal_ordinal_work[raw_dat$DD731=="Less than once a week"|raw_dat$DD731=="Once a week"|raw_dat$DD731=="2 times a week"]<-1
# raw_dat$temp_physact_mother_postnatal_ordinal_work[raw_dat$DD731=="3-4 times a week"|raw_dat$DD731=="5 times a week or more"]<-2 
# 
# 
# dat$physact_mother_postnatal_ordinal<-NA
# dat$physact_mother_postnatal_ordinal[raw_dat$temp_physact_mother_postnatal_ordinal_work==0 & raw_dat$temp_physact_mother_postnatal_ordinal_leisure==0]<-"None"
# dat$physact_mother_postnatal_ordinal[raw_dat$temp_physact_mother_postnatal_ordinal_work==1 | raw_dat$temp_physact_mother_postnatal_ordinal_leisure==1]<-"Light"
# dat$physact_mother_postnatal_ordinal[raw_dat$temp_physact_mother_postnatal_ordinal_work==2 | raw_dat$temp_physact_mother_postnatal_ordinal_leisure==2]<-"Moderate"
# dat$physact_mother_postnatal_ordinal<-factor(dat$physact_mother_postnatal_ordinal,levels=c("None","Light","Moderate"),ordered=T)
# 
# 
# #Physical activity postnatal binary
# dat$physact_mother_postnatal_binary<-NA
# dat$physact_mother_postnatal_binary[dat$physact_mother_postnatal_ordinal=="None"]<-0
# dat$physact_mother_postnatal_binary[dat$physact_mother_postnatal_ordinal=="Light"| dat$physact_mother_postnatal_ordinal=="Moderate"]<-1
# 
# ### Paternal
# 
# #Physical activity before pregnancy
#   ##NA
# #Physical activity in the first trimester
#   ##NA
# #Physical activity in the second trimester
# raw_dat$temp_physact_father_secondtrim_ordinal_leisure<-NA
# raw_dat$temp_physact_father_secondtrim_ordinal_leisure[qfar$FF246==1]<-0 # Never
# raw_dat$temp_physact_father_secondtrim_ordinal_leisure[qfar$FF246==2|qfar$FF246==3|qfar$FF246==4]<-1
# raw_dat$temp_physact_father_secondtrim_ordinal_leisure[qfar$FF246==6|qfar$FF246==5]<-2 
# raw_dat$temp_physact_father_secondtrim_ordinal_work<-NA
# raw_dat$temp_physact_father_secondtrim_ordinal_work[qfar$FF247==1]<-0 # Never
# raw_dat$temp_physact_father_secondtrim_ordinal_work[qfar$FF247==2|qfar$FF247==3|qfar$FF247==4]<-1 
# raw_dat$temp_physact_father_secondtrim_ordinal_work[qfar$FF247==6||qfar$FF247==5]<-2 
# 
# dat$physact_father_secondtrim_ordinal<-NA
# dat$physact_father_secondtrim_ordinal[raw_dat$temp_physact_father_secondtrim_ordinal_work==0 & raw_dat$temp_physact_father_secondtrim_ordinal_leisure==0]<-"None"
# dat$physact_father_secondtrim_ordinal[raw_dat$temp_physact_father_secondtrim_ordinal_work==1 | raw_dat$temp_physact_father_secondtrim_ordinal_leisure==1]<-"Light"
# dat$physact_father_secondtrim_ordinal[raw_dat$temp_physact_father_secondtrim_ordinal_work==2 | raw_dat$temp_physact_father_secondtrim_ordinal_leisure==2]<-"Moderate"
# dat$physact_father_secondtrim_ordinal<-factor(dat$physact_father_secondtrim_ordinal,levels=c("None","Light","Moderate"),ordered=T)
# 
# #Physical activity in the third trimester
#   ##NA
# #Physical activity at any time in pregnancy
# dat$physact_father_ever_pregnancy<-NA
# dat$physact_father_ever_pregnancy[dat$physact_father_secondtrim_ordinal=="None"]<-0
# dat$physact_father_ever_pregnancy[dat$physact_father_secondtrim_ordinal=="Light"| dat$physact_father_secondtrim_ordinal=="Moderate"]<-1
# 
# #Physical activity postnatally up to age 2
#   # NA
# ```
##################################################################
###############       Child outcomes              ################
##################################################################

# Ages are defined as:

# at birth or in first year of life (>0 to <1)
# childhood stage 1 (around age 2; >=1 to <3)
# childhood stage 2 (around age 4; >=3 to <5)
# childhood stage 3 (around age 6; >=5 to <8)
# childhood stage 4 (around age 9; >=8 to <11)

################################################
#####  Child's anthropometry and adiposity  ####
################################################

# Birthweight
dat$anthro_birthweight<-raw_dat$VEKT
dat$anthro_birthweight_zscore<-scale(dat$anthro_birthweight)

# Low BW <2500g vs >=2500g & <=4500g
dat$anthro_birthweight_low_binary<-NA
dat$anthro_birthweight_low_binary[dat$anthro_birthweight<2500]<-1
dat$anthro_birthweight_low_binary[dat$anthro_birthweight>=2500& dat$anthro_birthweight<=4500]<-0

# High BW >4500g vs >=2500g & <=4500g
dat$anthro_birthweight_high_binary<-NA
dat$anthro_birthweight_high_binary[dat$anthro_birthweight>4500]<-1
dat$anthro_birthweight_high_binary[dat$anthro_birthweight>=2500& dat$anthro_birthweight<=4500]<-0

# SGA

raw_dat$deciles2=NA
for(i in c("Male", "Female")){
  for(j in 20:44){
    print(paste("Running:", i, ", ", j))
    raw_dat$deciles2[which(raw_dat$KJONN == i & raw_dat$SVLEN == j)] <-
      cut(x=raw_dat$VEKT[which(raw_dat$KJONN == i & raw_dat$SVLEN == j)],
          breaks=quantile(raw_dat$VEKT[which(raw_dat$KJONN == i & raw_dat$SVLEN == j)],
                          probs = seq(0, 1, 0.1), na.rm = T))
  }
}


quantile(dat$anthro_birthweight, probs = seq(.1,.9, by = .1), na.rm = T)

dat$anthro_birthweight_sga_binary<-NA
dat$anthro_birthweight_sga_binary[raw_dat$deciles==1]<-1
dat$anthro_birthweight_sga_binary[raw_dat$deciles>1]<-0

# LGA
dat$anthro_birthweight_lga_binary<-NA
dat$anthro_birthweight_lga_binary[raw_dat$deciles==10]<-1
dat$anthro_birthweight_lga_binary[raw_dat$deciles<10]<-0

# Birthlength
## dont have access to this var (SETE_ISSE) in dataset

# Head circumference (cm)
## only available at birth and stage1
dat$anthro_head_circ_birth<-raw_dat$HODE
dat$anthro_head_circ_birth_zscore<-scale(dat$anthro_head_circ_birth)

temp8<-raw_dat$EE388
temp8[temp8<=0]<-NA
dat$anthro_head_circ_stage0 <- temp8
dat$anthro_head_circ_stage0[dat$anthro_head_circ_stage0<quantile(dat$anthro_head_circ_stage0,na.rm=T,probs=0.001)|dat$anthro_head_circ_stage0>quantile(dat$anthro_head_circ_stage0,na.rm=T,probs=0.999)]<-NA
dat$anthro_head_circ_stage0_zscore<-scale(dat$anthro_head_circ_stage0)
dat$stage0_timepoint_headcirc <-NA
dat$stage0_timepoint_headcirc[is.na(dat$anthro_head_circ_stage0)==F]<-8

temp12<-raw_dat$EE394
temp12[temp12<=0]<-NA
dat$anthro_head_circ_stage1 <- temp12
dat$anthro_head_circ_stage1[dat$anthro_head_circ_stage1<quantile(dat$anthro_head_circ_stage1,na.rm=T,probs=0.001)|dat$anthro_head_circ_stage1>quantile(dat$anthro_head_circ_stage1,na.rm=T,probs=0.999)]<-NA
dat$anthro_head_circ_stage1_zscore<-scale(dat$anthro_head_circ_stage1)
dat$stage1_timepoint_headcirc <-NA
dat$stage1_timepoint_headcirc[is.na(dat$anthro_head_circ_stage1)==F]<-12

rm(temp8,temp12,temp8112)

# Height - take the latest timepoint and supplement with earlier timepoints if NA
height_temp8<-raw_dat$EE387
height_temp8[height_temp8<=0]<-NA
dat$anthro_height_stage0 <- height_temp8
dat$anthro_height_stage0[dat$anthro_height_stage0<quantile(dat$anthro_height_stage0,na.rm=T,probs=0.001)|dat$anthro_height_stage0>quantile(dat$anthro_height_stage0,na.rm=T,probs=0.999)]<-NA
dat$anthro_height_stage0_zscore<-scale(dat$anthro_height_stage0)
dat$stage0_timepoint_height <-NA
dat$stage0_timepoint_height[is.na(dat$anthro_height_stage0)==F]<-8

height_temp12<-raw_dat$EE393
height_temp12[height_temp12<=0]<-NA
height_temp15<-raw_dat$EE399
height_temp15[height_temp15<=0]<-NA
height_temp18<-raw_dat$GG15
height_temp18[height_temp18<=0]<-NA
height_temp24<-raw_dat$GG20
height_temp24[height_temp24<=0]<-NA

height_timepoint<-ifelse(is.na(height_temp24),NA,"24")
height_temp_all<-height_temp24 # use latest timepoint
height_temp_all[is.na(height_timepoint)]<-height_temp18[is.na(height_timepoint)] # if NA use 18m
height_timepoint[is.na(height_timepoint)&(is.na(height_temp18)==FALSE)]<-"18"
height_temp_all[is.na(height_timepoint)]<-height_temp15[is.na(height_timepoint)] # if NA use 15
height_timepoint[is.na(height_timepoint)&(is.na(height_temp15)==FALSE)]<-"15"
height_temp_all[is.na(height_timepoint)]<-height_temp12[is.na(height_timepoint)] # if NA use 12
height_timepoint[is.na(height_timepoint)&(is.na(height_temp12)==FALSE)]<-"12"

dat$stage1_timepoint_height<-height_timepoint
dat$anthro_height_stage1<-height_temp_all

dat$anthro_height_stage1[dat$anthro_height_stage1<quantile(dat$anthro_height_stage1,na.rm=T,probs=0.001)|dat$anthro_height_stage1>quantile(dat$anthro_height_stage1,na.rm=T,probs=0.999)]<-NA
dat$anthro_height_stage1_zscore<-scale(dat$anthro_height_stage1)
  
dat$anthro_height_stage2<-raw_dat$GG25
dat$anthro_height_stage2[dat$anthro_height_stage2<quantile(dat$anthro_height_stage2,na.rm=T,probs=0.001)|dat$anthro_height_stage2>quantile(dat$anthro_height_stage2,na.rm=T,probs=0.999)]<-NA
dat$anthro_height_stage2_zscore<-scale(dat$anthro_height_stage2)

dat$anthro_height_stage3<-raw_dat$LL12
dat$anthro_height_stage3[dat$anthro_height_stage3<quantile(dat$anthro_height_stage3,na.rm=T,probs=0.001)|dat$anthro_height_stage3>quantile(dat$anthro_height_stage3,na.rm=T,probs=0.999)]<-NA
dat$anthro_height_stage3_zscore<-scale(dat$anthro_height_stage3)

dat$anthro_height_stage4<-raw_dat$NN24
dat$anthro_height_stage4[dat$anthro_height_stage4<quantile(dat$anthro_height_stage4,na.rm=T,probs=0.001)|dat$anthro_height_stage4>quantile(dat$anthro_height_stage4,na.rm=T,probs=0.999)]<-NA
dat$anthro_height_stage4_zscore<-scale(dat$anthro_height_stage4)

# Waist circumference
  ## NA

# Weight (kg)
weight_temp8<-raw_dat$EE386/1000
weight_temp8[weight_temp8<=0]<-NA
dat$anthro_weight_stage0 <- weight_temp8
dat$anthro_weight_stage0[dat$anthro_weight_stage0<quantile(dat$anthro_weight_stage0,na.rm=T,probs=0.001)|dat$anthro_weight_stage0>quantile(dat$anthro_weight_stage0,na.rm=T,probs=0.999)]<-NA
dat$anthro_weight_stage0_zscore<-scale(dat$anthro_weight_stage0)
dat$stage0_timepoint_weight <-NA
dat$stage0_timepoint_weight[is.na(dat$anthro_weight_stage0)==F]<-8

weight_temp12<-raw_dat$EE392/1000
weight_temp12[weight_temp12<=0]<-NA
weight_temp15<-raw_dat$EE398/1000
weight_temp15[weight_temp15<=0]<-NA
weight_temp18<-raw_dat$GG16
weight_temp18[weight_temp18<=0]<-NA
weight_temp24<-raw_dat$GG21
weight_temp24[weight_temp24<=0]<-NA

weight_timepoint<-ifelse(is.na(weight_temp24),NA,"24")
weight_temp_all<-weight_temp24 # use latest timepoint
weight_temp_all[is.na(weight_timepoint)]<-weight_temp18[is.na(weight_timepoint)]
weight_timepoint[is.na(weight_timepoint)&(is.na(weight_temp18)==FALSE)]<-"18"
weight_temp_all[is.na(weight_timepoint)]<-weight_temp15[is.na(weight_timepoint)]
weight_timepoint[is.na(weight_timepoint)&(is.na(weight_temp15)==FALSE)]<-"15"
weight_temp_all[is.na(weight_timepoint)]<-weight_temp12[is.na(weight_timepoint)] # if NA use 43m
weight_timepoint[is.na(weight_timepoint)&(is.na(weight_temp12)==FALSE)]<-"12"

dat$stage1_timepoint_weight<-weight_timepoint
dat$anthro_weight_stage1<-weight_temp_all

dat$anthro_weight_stage1[dat$anthro_weight_stage1<quantile(dat$anthro_weight_stage1,na.rm=T,probs=0.001)|dat$anthro_weight_stage1>quantile(dat$anthro_weight_stage1,na.rm=T,probs=0.999)]<-NA
dat$anthro_weight_stage1[dat$anthro_weight_stage1<5]<-NA
dat$anthro_weight_stage1_zscore<-scale(dat$anthro_weight_stage1)

dat$anthro_weight_stage2<-raw_dat$GG26
dat$anthro_weight_stage2[dat$anthro_weight_stage2<quantile(dat$anthro_weight_stage2,na.rm=T,probs=0.001)|dat$anthro_weight_stage2>quantile(dat$anthro_weight_stage2,na.rm=T,probs=0.999)]<-NA
dat$anthro_weight_stage2_zscore<-scale(dat$anthro_weight_stage2)

## adding any variables from age 7 as done above) 
dat$anthro_weight_stage3<-raw_dat$LL13
dat$anthro_weight_stage3[dat$anthro_weight_stage3<quantile(dat$anthro_weight_stage3,na.rm=T,probs=0.001)|dat$anthro_weight_stage3>quantile(dat$anthro_weight_stage3,na.rm=T,probs=0.999)]<-NA
dat$anthro_weight_stage3_zscore<-scale(dat$anthro_weight_stage3)

dat$anthro_weight_stage4<-raw_dat$NN25
dat$anthro_weight_stage4[dat$anthro_weight_stage4<quantile(dat$anthro_weight_stage4,na.rm=T,probs=0.001)|dat$anthro_weight_stage4>quantile(dat$anthro_weight_stage4,na.rm=T,probs=0.999)]<-NA
dat$anthro_weight_stage4_zscore<-scale(dat$anthro_weight_stage4)


# BMI (kg/m2)

dat$anthro_bmi_stage0<-dat$anthro_weight_stage0 /((dat$anthro_height_stage0/100)^2)
dat$stage0_timepoint_bmi <- 8

calc_bmi_stage1 <- function(age){
  H <- height_temp_all
  W <- weight_temp_all
  BMI <- W / ((H/100)^2)
  BMI[height_timepoint!=age & weight_timepoint!=age] <- NA
  BMI
}

bmi_temp24 <- calc_bmi_stage1("24")
bmi_temp18 <- calc_bmi_stage1("18")
bmi_temp15 <- calc_bmi_stage1("15")
bmi_temp12 <- calc_bmi_stage1("12")
bmi_temp8 <- calc_bmi_stage1("8")

bmi_timepoint<-ifelse(is.na(bmi_temp24),NA,"24")
bmi_temp_all<-bmi_temp24 # use latest timepoint
bmi_temp_all[is.na(bmi_timepoint)]<-bmi_temp18[is.na(bmi_timepoint)]
bmi_timepoint[is.na(bmi_timepoint)&(is.na(bmi_temp18)==FALSE)]<-"18"
bmi_temp_all[is.na(bmi_timepoint)]<-bmi_temp15[is.na(bmi_timepoint)]
bmi_timepoint[is.na(bmi_timepoint)&(is.na(bmi_temp15)==FALSE)]<-"15"
bmi_temp_all[is.na(bmi_timepoint)]<-bmi_temp12[is.na(bmi_timepoint)] # if NA use 43m
bmi_timepoint[is.na(bmi_timepoint)&(is.na(bmi_temp12)==FALSE)]<-"12"

dat$anthro_bmi_stage1 <- bmi_temp_all
dat$stage1_timepoint_bmi <- as.numeric(bmi_timepoint)
dat$anthro_bmi_stage1[dat$anthro_bmi_stage1<quantile(dat$anthro_bmi_stage1,na.rm=T,probs=0.005)|dat$anthro_bmi_stage1>quantile(dat$anthro_bmi_stage1,na.rm=T,probs=0.999)]<-NA

dat$anthro_bmi_stage1_zscore<-scale(dat$anthro_bmi_stage1)

dat$anthro_bmi_stage2<-dat$anthro_weight_stage2 /((dat$anthro_height_stage2/100)^2)
dat$anthro_bmi_stage3<-dat$anthro_weight_stage3 /((dat$anthro_height_stage3/100)^2)
dat$anthro_bmi_stage4<-dat$anthro_weight_stage4 /((dat$anthro_height_stage4/100)^2)
dat$anthro_bmi_stage0_zscore<-scale(dat$anthro_bmi_stage0)
dat$anthro_bmi_stage1_zscore<-scale(dat$anthro_bmi_stage1)
dat$anthro_bmi_stage2_zscore<-scale(dat$anthro_bmi_stage2)
dat$anthro_bmi_stage3_zscore<-scale(dat$anthro_bmi_stage3)
dat$anthro_bmi_stage4_zscore<-scale(dat$anthro_bmi_stage4)

# WHO categories of overwight/obese/underweight vs normal weight
  ## NA 
dat$anthro_bmi_stage0_sds <- sds(value=dat$anthro_bmi_stage0,
                                 age=dat$stage0_timepoint_bmi/12,
                                 sex=dat$covs_sex,male="male",female="female",
                                 ref = who.ref,item='bmi')
dat$anthro_bmi_stage0_sds[dat$anthro_bmi_stage0_sds%in%c(Inf,-Inf)]<-NA

dat$anthro_bmi_stage1_sds <- sds(value=dat$anthro_bmi_stage1,
                                 age=dat$stage1_timepoint_bmi/12,
                                 sex=dat$covs_sex,male="male",female="female",
                                 ref = ukwho.ref,item='bmi')
dat$anthro_bmi_stage1_sds[dat$anthro_bmi_stage1_sds%in%c(Inf,-Inf)]<-NA

dat$anthro_bmi_stage2_sds <- sds(value=dat$anthro_bmi_stage2,
                                 age=dat$covs_age_child_stage2/12,
                                 sex=dat$covs_sex,male="male",female="female",
                                 ref = ukwho.ref,item='bmi')
dat$anthro_bmi_stage2_sds[dat$anthro_bmi_stage2_sds%in%c(Inf,-Inf)]<-NA

dat$anthro_bmi_stage3_sds <- sds(value=dat$anthro_bmi_stage3,
                                 age=dat$covs_age_child_stage3/12,
                                 sex=dat$covs_sex,male="male",female="female",
                                 ref = ukwho.ref,item='bmi')
dat$anthro_bmi_stage3_sds[dat$anthro_bmi_stage3_sds%in%c(Inf,-Inf)]<-NA

dat$anthro_bmi_stage4_sds <- sds(value=dat$anthro_bmi_stage4,
                                 age=dat$covs_age_child_stage4/12,
                                 sex=dat$covs_sex,male="male",female="female",
                                 ref = ukwho.ref,item='bmi')
dat$anthro_bmi_stage4_sds[dat$anthro_bmi_stage4_sds%in%c(Inf,-Inf)]<-NA

dat$anthro_obese_stage0_binary <-NA
dat$anthro_overweightobese_stage0_binary <-NA
dat$anthro_obese_stage0_binary[dat$anthro_bmi_stage0_sds>2]<-1
dat$anthro_overweightobese_stage0_binary[dat$anthro_bmi_stage0_sds>1]<-1
dat$anthro_obese_stage0_binary[dat$anthro_bmi_stage0_sds<=1 & dat$anthro_bmi_stage0_sds>=(-2)]<-0
dat$anthro_overweightobese_stage0_binary[dat$anthro_bmi_stage0_sds<=1 & dat$anthro_bmi_stage0_sds>=(-2)]<-0

dat$anthro_obese_stage1_binary <-NA
dat$anthro_overweightobese_stage1_binary <-NA
dat$anthro_obese_stage1_binary[dat$anthro_bmi_stage1_sds>2]<-1
dat$anthro_overweightobese_stage1_binary[dat$anthro_bmi_stage1_sds>1]<-1
dat$anthro_obese_stage1_binary[dat$anthro_bmi_stage1_sds<=1 & dat$anthro_bmi_stage1_sds>=(-2)]<-0
dat$anthro_overweightobese_stage1_binary[dat$anthro_bmi_stage1_sds<=1 & dat$anthro_bmi_stage1_sds>=(-2)]<-0

dat$anthro_obese_stage2_binary <-NA
dat$anthro_overweightobese_stage2_binary <-NA
dat$anthro_obese_stage2_binary[dat$anthro_bmi_stage2_sds>2]<-1
dat$anthro_overweightobese_stage2_binary[dat$anthro_bmi_stage2_sds>1]<-1
dat$anthro_obese_stage2_binary[dat$anthro_bmi_stage2_sds<=1 & dat$anthro_bmi_stage2_sds>=(-2)]<-0
dat$anthro_overweightobese_stage2_binary[dat$anthro_bmi_stage2_sds<=1 & dat$anthro_bmi_stage2_sds>=(-2)]<-0

dat$anthro_obese_stage3_binary <-NA
dat$anthro_overweightobese_stage3_binary <-NA
dat$anthro_obese_stage3_binary[dat$anthro_bmi_stage3_sds>2]<-1
dat$anthro_overweightobese_stage3_binary[dat$anthro_bmi_stage3_sds>1]<-1
dat$anthro_obese_stage3_binary[dat$anthro_bmi_stage3_sds<=1 & dat$anthro_bmi_stage3_sds>=(-2)]<-0
dat$anthro_overweightobese_stage3_binary[dat$anthro_bmi_stage3_sds<=1 & dat$anthro_bmi_stage3_sds>=(-2)]<-0

dat$anthro_obese_stage4_binary <-NA
dat$anthro_overweightobese_stage4_binary <-NA
dat$anthro_obese_stage4_binary[dat$anthro_bmi_stage4_sds>2]<-1
dat$anthro_overweightobese_stage4_binary[dat$anthro_bmi_stage4_sds>1]<-1
dat$anthro_obese_stage4_binary[dat$anthro_bmi_stage4_sds<=1 & dat$anthro_bmi_stage4_sds>=(-2)]<-0
dat$anthro_overweightobese_stage4_binary[dat$anthro_bmi_stage4_sds<=1 & dat$anthro_bmi_stage4_sds>=(-2)]<-0

# Fat mass index
  ## NA

################################################################
####    child's psychosocial and cognitive outcomes         ####
################################################################

##
# Total behavioural difficulties

# function if any numerical
cbcl_funct_numb<-function(var){
  newvar<-var-1
  newvar[which(newvar<0)]<-NA
  return(newvar)
}

cbcl_funct_minus1<-function(var){
  newvar<-rep(NA,length(var))
  newvar[which(var=="Not true")]<-0
  newvar[which(var=="Somewhat or sometimes true")]<-1
  newvar[which(var=="Very true or often true")]<-2
  return(newvar)
}

cbcl_funct_5yr<-function(var){
  newvar<-rep(NA,length(var))
  newvar[which(var=="Rarely/never")]<-0
  newvar[which(var=="Sometimes")]<-1
  newvar[which(var=="Often/ typical")]<-2
  return(newvar)
}

 #18 months 
for(var in c("EE435", "EE961", "EE903", "EE904", "EE438", "EE439", "EE962", "EE442", "EE446","EE447", "EE448", "EE963", "EE964", "EE906", "EE440", "EE907","EE908","EE909")){
  eval(parse(text=paste("raw_dat$temp_", var, " <- cbcl_funct_minus1(var=raw_dat$", var, ")", sep="")))
}

#reversed coded so separate 
raw_dat$temp_EE905<-NA
raw_dat$temp_EE905[raw_dat$EE905=="Not true"]<-2
raw_dat$temp_EE905[raw_dat$EE905=="Somewhat or sometimes true"]<-1
raw_dat$temp_EE905[raw_dat$EE905=="Very true or often true"]<-0


#36 months (key stage 2)
for(var in c("GG313","GG314","GG315","GG316","GG317","GG318","GG319","GG320","GG321","GG322","GG323","GG324","GG325","GG326","GG327","GG328",
             "GG329","GG330","GG331","GG332","GG333","GG334","GG335","GG336","GG337","GG338")){
  eval(parse(text=paste("raw_dat$temp_", var, " <- cbcl_funct_minus1(var=raw_dat$", var, ")", sep="")))
}

# 5 year (stage 3)
for(var in c("LL301","LL302","LL303","LL304","LL305", "LL306","LL307","LL308","LL309","LL310","LL311","LL312","LL313","LL314","LL315","LL316",
             "LL317","LL318","LL319","LL320","LL321","LL322","LL323","LL324","LL325","LL504","LL505","LL329")){
  eval(parse(text=paste("raw_dat$temp_", var, " <- cbcl_funct_5yr(var=raw_dat$", var, ")", sep="")))
}

cbcl_variables <- list(total1=c("temp_EE435", "temp_EE961", "temp_EE903", "temp_EE904", "temp_EE438",
                                "temp_EE439", "temp_EE962", "temp_EE442", "temp_EE446","temp_EE446", "temp_EE448",
                                "temp_EE963", "temp_EE964", "temp_EE906", "temp_EE440", "temp_EE907", "temp_EE908",
                                "temp_EE909", "temp_EE905"),
                       total2=c("temp_GG313","temp_GG314","temp_GG315","temp_GG316","temp_GG317","temp_GG318","temp_GG319","temp_GG320","temp_GG321","temp_GG322",
                                "temp_GG323","temp_GG324","temp_GG325","temp_GG326","temp_GG327","temp_GG328",
                                "temp_GG329","temp_GG330","temp_GG331","temp_GG332","temp_GG333","temp_GG334",
                                "temp_GG335","temp_GG336","temp_GG337","temp_GG338"),
                       total3=c("temp_LL301","temp_LL302","temp_LL303","temp_LL304","temp_LL305", "temp_LL306","temp_LL307","temp_LL308","temp_LL309","temp_LL310","temp_LL311","temp_LL312","temp_LL313","temp_LL314","temp_LL315","temp_LL316",
                                "temp_LL317","temp_LL318","temp_LL319","temp_LL320","temp_LL321","temp_LL322","temp_LL323","temp_LL324","temp_LL325","temp_LL504","temp_LL505"),
                       hyperactivity1=c("temp_EE435","temp_EE961","temp_EE903","temp_EE904"),
                       hyperactivity2=c("temp_GG314","temp_GG332","temp_GG315","temp_GG327"),
                       hyperactivity3=c("temp_LL302","temp_LL319","temp_LL303","temp_LL314"),
                       aggression1=c("temp_EE446","temp_EE447","temp_EE962","temp_EE442","temp_EE448"),
                       aggression2=c("temp_GG319","temp_GG324","temp_GG326","temp_GG319","temp_GG331"),
                       aggression3=c("temp_LL307","temp_LL311","temp_LL313","temp_LL329","temp_LL318"),
                       internalising1=c("temp_EE438","temp_EE439","temp_EE440","temp_EE908","temp_EE909","temp_EE963"),
                       internalising2=c("temp_GG317","temp_GG328","temp_GG336","temp_GG321","temp_GG335","temp_GG323","temp_GG334","temp_GG337","temp_GG318"),
                       internalising3=c("temp_LL305","temp_LL315","temp_LL321","temp_LL504","temp_LL317","temp_LL505", "temp_LL322","temp_LL309","temp_LL310", "temp_LL320","temp_LL323"),
                      externalising1=c("temp_EE435","temp_EE961","temp_EE903","temp_EE904","temp_EE962","temp_EE442","temp_EE446","temp_EE447","temp_EE448"),
                      externalising2=c("temp_GG314","temp_GG315", "temp_GG316", "temp_GG319", "temp_GG320", "temp_GG324","temp_GG326","temp_GG329","temp_GG330", "temp_GG331","temp_GG332"), 
                      externalising3=c("temp_LL302","temp_LL303","temp_LL304","temp_LL308","temp_LL314","temp_LL319","temp_LL307","temp_LL311","temp_LL313","temp_LL318","temp_LL329")
)

calc_cbcl <- function(varnames){
  var <- rowSums(raw_dat[,varnames], na.rm=TRUE)
  var[apply(raw_dat[,varnames],1,function(x)all(is.na(x)))] <- NA
  var
}

cbcl_scores <- lapply(cbcl_variables,calc_cbcl)

dat$neuro_totalcbcl_stage1<-cbcl_scores[[1]]
dat$neuro_totalcbcl_stage1_zscore<-scale(dat$neuro_totalcbcl_stage1)
dat$neuro_totalcbcl_stage2<-cbcl_scores[[2]]
dat$neuro_totalcbcl_stage2_zscore<-scale(dat$neuro_totalcbcl_stage2)
dat$neuro_totalcbcl_stage3<-cbcl_scores[[3]]
dat$neuro_totalcbcl_stage3_zscore<-scale(dat$neuro_totalcbcl_stage3)

dat$neuro_internalising_stage1<-cbcl_scores[[10]]
dat$neuro_internalising_stage1_zscore<-scale(dat$neuro_internalising_stage1)
dat$neuro_internalising_stage2<-cbcl_scores[[11]]
dat$neuro_internalising_stage2_zscore<-scale(dat$neuro_internalising_stage2)
dat$neuro_internalising_stage3<-cbcl_scores[[12]]
dat$neuro_internalising_stage3_zscore<-scale(dat$neuro_internalising_stage3)

dat$neuro_externalising_stage1<-cbcl_scores[[13]]
dat$neuro_externalising_stage1_zscore<-scale(dat$neuro_externalising_stage1)
dat$neuro_externalising_stage2<-cbcl_scores[[14]]
dat$neuro_externalising_stage2_zscore<-scale(dat$neuro_externalising_stage2)
dat$neuro_externalising_stage3<-cbcl_scores[[15]]
dat$neuro_externalising_stage3_zscore<-scale(dat$neuro_externalising_stage3)

dat$neuro_hyperactive_stage1<-cbcl_scores[[4]]
dat$neuro_hyperactive_stage1_zscore<-scale(dat$neuro_hyperactive_stage1)
dat$neuro_hyperactive_stage2<-cbcl_scores[[5]]
dat$neuro_hyperactive_stage2_zscore<-scale(dat$neuro_hyperactive_stage2)
dat$neuro_hyperactive_stage3<-cbcl_scores[[6]]
dat$neuro_hyperactive_stage3_zscore<-scale(dat$neuro_hyperactive_stage3)

dat$neuro_aggressive_stage1<-cbcl_scores[[7]]
dat$neuro_aggressive_stage1_zscore<-scale(dat$neuro_aggressive_stage1)
dat$neuro_aggressive_stage2<-cbcl_scores[[8]]
dat$neuro_aggressive_stage2_zscore<-scale(dat$neuro_aggressive_stage2)
dat$neuro_aggressive_stage3<-cbcl_scores[[9]]
dat$neuro_aggressive_stage3_zscore<-scale(dat$neuro_aggressive_stage3)

# IQ and other composite measures of intelligence
# Educational attainment
# Depression

# 5 year (stage 3)
mfq_funct_5yr<-function(var){
  newvar<-rep(NA,length(var))
  newvar[which(var=="Disagree")]<-0
  newvar[which(var=="Sometimes correct")]<-1
  newvar[which(var=="Correct")]<-2
  return(newvar)
}

for(var in c("NN68","NN69","NN70","NN71","NN72","NN73","NN74","NN75","NN76","NN77","NN78","NN79","NN80")){
  eval(parse(text=paste("raw_dat$temp_", var, " <- mfq_funct_5yr(var=raw_dat$", var, ")", sep="")))
}

mfq <- rowSums(raw_dat[,c("temp_NN68","temp_NN69","temp_NN70","temp_NN71","temp_NN72","temp_NN73","temp_NN74",
                         "temp_NN75","temp_NN76","temp_NN77","temp_NN78","temp_NN79","temp_NN80")], na.rm=TRUE)
  mfq[apply(raw_dat[,c("temp_NN68","temp_NN69","temp_NN70","temp_NN71","temp_NN72","temp_NN73","temp_NN74",
                    "temp_NN75","temp_NN76","temp_NN77","temp_NN78","temp_NN79","temp_NN80")],1,function(x)all(is.na(x)))] <- NA

dat$neuro_depression_stage4<-mfq
dat$neuro_depression_stage4_zscore<-scale(dat$neuro_depression_stage4)

# Prosocial behaviour

prosocial_funct<-function(var){
  newvar<-rep(NA,length(var))
  newvar[which(var=="Disagree")]<-0
  newvar[which(var=="Partially agree")]<-1
  newvar[which(var=="Totally agree")]<-2
  return(newvar)
}

for(var in c("GG231","GG232","GG233","GG234","GG235")){
  eval(parse(text=paste("raw_dat$temp_", var, " <- prosocial_funct(var=raw_dat$", var, ")", sep="")))
}

dat$neuro_prosocial_stage2<-rowSums(raw_dat[,c("temp_GG231","temp_GG232","temp_GG233","temp_GG234","temp_GG235")], na.rm=TRUE)
dat$neuro_prosocial_stage2[apply(raw_dat[,c("temp_GG231","temp_GG232","temp_GG233","temp_GG234","temp_GG235")],1,function(x)all(is.na(x)))] <- NA

dat$neuro_prosocial_stage2_zscore<-scale(dat$neuro_prosocial_stage2)

dat$neuro_prosocial_stage2_binary<-NA
dat$neuro_prosocial_stage2_binary[dat$neuro_prosocial_stage2>6]<-0
dat$neuro_prosocial_stage2_binary[dat$neuro_prosocial_stage2<=6]<-1

## Autism

#Stage 1 is M-CHAT measure. Stages 2 and 3 are SCQ and therefore scored differently

binary_yn_funct<-function(var){
  newvar<-rep(NA,length(var))
  newvar[which(var=="No")]<-0
  newvar[which(var=="Yes")]<-1
  return(newvar)
}

binary_reversed_funct<-function(var){
  newvar<-rep(NA,length(var))
  newvar[which(var=="No")]<-1
  newvar[which(var=="Yes")]<-0
  return(newvar)
}

for(var in c("EE427","EE1005","EE434","EE429", "EE430","EE996","EE431", "EE998","EE432",
             "EE997","EE433","EE428","EE1006","EE1000","EE879","EE901","EE882","EE986","EE406",
             "EE1001","EE881","EE899","EE902")){
  eval(parse(text=paste("raw_dat$temp_", var, " <- binary_reversed_funct(var=raw_dat$", var, ")", sep="")))
}

  
#reverse scored
raw_dat$temp_EE900<-NA
raw_dat$temp_EE900[raw_dat$EE900=="No"]<-0
raw_dat$temp_EE900[raw_dat$EE900=="Yes"]<-1

raw_dat$temp_EE880<-NA
raw_dat$temp_EE880[raw_dat$EE880=="No"]<-0
raw_dat$temp_EE880[raw_dat$EE880=="Yes"]<-1

raw_dat$temp_EE1002<-NA
raw_dat$temp_EE1002[raw_dat$EE1002=="No"]<-0
raw_dat$temp_EE1002[raw_dat$EE1002=="Yes"]<-1

raw_dat$temp_EE883<-NA
raw_dat$temp_EE883[raw_dat$EE883=="No"]<-0
raw_dat$temp_EE883[raw_dat$EE883=="Yes"]<-1

dat$neuro_autism_stage1<-rowSums(raw_dat[,c("temp_EE427","temp_EE1005","temp_EE434","temp_EE429", "temp_EE430","temp_EE996",
                                             "temp_EE431", "temp_EE998","temp_EE432",
                                            "temp_EE997","temp_EE433","temp_EE428","temp_EE1006","temp_EE1000","temp_EE879","temp_EE901",
                                            "temp_EE882","temp_EE986","temp_EE406",
                                            "temp_EE1001","temp_EE881","temp_EE899","temp_EE902","temp_EE900","temp_EE880","temp_EE1002","temp_EE883")], na.rm=TRUE)
dat$neuro_autism_stage1[apply(raw_dat[,c("temp_EE427","temp_EE1005","temp_EE434","temp_EE429", "temp_EE430","temp_EE996",
                                         "temp_EE431", "temp_EE998","temp_EE432",
                                         "temp_EE997","temp_EE433","temp_EE428","temp_EE1006","temp_EE1000","temp_EE879","temp_EE901",
                                         "temp_EE882","temp_EE986","temp_EE406",
                                         "temp_EE1001","temp_EE881","temp_EE899","temp_EE902","temp_EE900","temp_EE880","temp_EE1002","temp_EE883")],1,function(x)all(is.na(x)))] <- NA


#7 is cut off for the MCHAT scale
dat$neuro_autism_stage1_binary<-NA
dat$neuro_autism_stage1_binary[dat$neuro_autism_stage1<7]<-0
dat$neuro_autism_stage1_binary[dat$neuro_autism_stage1>=7]<-1


#autism stage 2

#these are the questions where a positive (yes) answer would indicate a non-autistic behaviour/trait
#so yes is set to 0 and no is set to 1

for(var in c("GG257", "GG264","GG274","GG256","GG275", "GG276","GG277","GG278","GG279","GG280","GG281","GG282","GG283",
"GG284","GG285","GG286","GG287","GG288","GG289","GG290","GG291","GG292","GG293","GG294",
"NN151","NN158","NN168","NN169","NN170","NN171","NN172","NN173","NN174","NN175","NN176","NN177","NN178","NN179","NN180",
"NN181","NN182","NN183","NN184","NN185","NN186","NN187","NN188","NN189")){
  eval(parse(text=paste("raw_dat$temp_", var, " <- binary_reversed_funct(var=raw_dat$", var, ")", sep="")))
}

# for these questions, a positive (yes) indicates an autistic behaviour/trait, so yes is set to 1 and no is set to 0

for(var in c("GG258","GG259","GG260","GG261","GG262","GG263","GG265","GG266","GG267","GG268","GG269","GG270","GG271","GG272","GG273",
             "NN152","NN153","NN154","NN155","NN156","NN157","NN159","NN160","NN161","NN162","NN163","NN164","NN165","NN166","NN167"
             )){
  eval(parse(text=paste("raw_dat$temp_", var, " <- binary_yn_funct(var=raw_dat$", var, ")", sep="")))
}

dat$neuro_autism_stage2<-rowSums(raw_dat[,c("temp_GG257", "temp_GG264","temp_GG274","temp_GG256","temp_GG275", "temp_GG276","temp_GG277","temp_GG278","temp_GG279","temp_GG280","temp_GG281","temp_GG282","temp_GG283",
                                            "temp_GG284","temp_GG285","temp_GG286","temp_GG287","temp_GG288","temp_GG289","temp_GG290","temp_GG291","temp_GG292","temp_GG293","temp_GG294","temp_GG258","temp_GG259",
                                            "temp_GG260","temp_GG261","temp_GG262","temp_GG263","temp_GG265","temp_GG266","temp_GG267","temp_GG268","temp_GG269","temp_GG270","temp_GG271","temp_GG272","temp_GG273")], na.rm=TRUE)
dat$neuro_autism_stage2[apply(raw_dat[,c("temp_GG257", "temp_GG264","temp_GG274","temp_GG256","temp_GG275", "temp_GG276","temp_GG277","temp_GG278","temp_GG279","temp_GG280","temp_GG281","temp_GG282","temp_GG283",
                                         "temp_GG284","temp_GG285","temp_GG286","temp_GG287","temp_GG288","temp_GG289","temp_GG290","temp_GG291","temp_GG292","temp_GG293","temp_GG294","temp_GG258","temp_GG259",
                                         "temp_GG260","temp_GG261","temp_GG262","temp_GG263","temp_GG265","temp_GG266","temp_GG267","temp_GG268","temp_GG269","temp_GG270","temp_GG271","temp_GG272","temp_GG273")],1,function(x)all(is.na(x)))] <- NA



#15 is cut off for the SCQ scale
dat$neuro_autism_stage2_binary<-NA
dat$neuro_autism_stage2_binary[dat$neuro_autism_stage2<15]<-0
dat$neuro_autism_stage2_binary[dat$neuro_autism_stage2>=15]<-1

##stage3
dat$neuro_autism_stage3<-rowSums(raw_dat[,c("temp_NN151","temp_NN158","temp_NN168","temp_NN169","temp_NN170","temp_NN171","temp_NN172","temp_NN173","temp_NN174","temp_NN175","temp_NN176","temp_NN177","temp_NN178",
                                            "temp_NN179","temp_NN180","temp_NN181","temp_NN182","temp_NN183","temp_NN184","temp_NN185","temp_NN186","temp_NN187","temp_NN188","temp_NN189","temp_NN152","temp_NN153",
                                            "temp_NN154","temp_NN155","temp_NN156","temp_NN157","temp_NN159","temp_NN160","temp_NN161","temp_NN162","temp_NN163","temp_NN164","temp_NN165","temp_NN166","temp_NN167")], na.rm=TRUE)
dat$neuro_autism_stage3[apply(raw_dat[,c("temp_NN151","temp_NN158","temp_NN168","temp_NN169","temp_NN170","temp_NN171","temp_NN172","temp_NN173","temp_NN174","temp_NN175","temp_NN176","temp_NN177","temp_NN178",
                                         "temp_NN179","temp_NN180","temp_NN181","temp_NN182","temp_NN183","temp_NN184","temp_NN185","temp_NN186","temp_NN187","temp_NN188","temp_NN189","temp_NN152","temp_NN153",
                                         "temp_NN154","temp_NN155","temp_NN156","temp_NN157","temp_NN159","temp_NN160","temp_NN161","temp_NN162","temp_NN163","temp_NN164","temp_NN165","temp_NN166","temp_NN167")],1,function(x)all(is.na(x)))] <- NA


#15 is cut off for the SCQ scale
dat$neuro_autism_stage3_binary<-NA
dat$neuro_autism_stage3_binary[dat$neuro_autism_stage3<15]<-0
dat$neuro_autism_stage3_binary[dat$neuro_autism_stage3>=15]<-1

dat$neuro_autism_stage1_zscore<-scale(dat$neuro_autism_stage1)
dat$neuro_autism_stage2_zscore<-scale(dat$neuro_autism_stage2)
dat$neuro_autism_stage3_zscore<-scale(dat$neuro_autism_stage3)

# Social communication development score 'Ages and Stages' measure
comm_6m_funct<-function(var){
  newvar<-rep(NA,length(var))
  newvar[which(var=="No, not yet")]<-0
  newvar[which(var=="Yes, but seldom")]<-1
  newvar[which(var=="Yes, often")]<-1

  return(newvar)
}

comm_18mplus_funct<-function(var){
  newvar<-rep(NA,length(var))
  newvar[which(var=="Not yet")]<-0
  newvar[which(var=="Sometimes")]<-1
  newvar[which(var=="Yes")]<-1
  
  return(newvar)
}


##nb the incorect spelling of 'regularily' here is correct, it's how it's originally coded
comm_8yr_funct<-function(var){
  newvar<-rep(NA,length(var))
  newvar[which(var=="Seldom or never")]<-0
  newvar[which(var=="Sometimes")]<-1
  newvar[which(var=="Regularily")]<-2
  newvar[which(var=="Often/ Always")]<-3
  
  return(newvar)
}

comm_8yr_reverse_funct<-function(var){
  newvar<-rep(NA,length(var))
  newvar[which(var=="Seldom or never")]<-3
  newvar[which(var=="Sometimes")]<-2
  newvar[which(var=="Regularily")]<-1
  newvar[which(var=="Often/ Always")]<-0
  
  return(newvar)
}

for(var in c("DD351","DD352","DD353","DD354","DD355")){
  eval(parse(text=paste("raw_dat$temp_", var, " <- comm_6m_funct(var=raw_dat$", var, ")", sep="")))
}

for(var in c("EE403","EE404","EE405")){
  eval(parse(text=paste("raw_dat$temp_", var, " <- comm_18mplus_funct(var=raw_dat$", var, ")", sep="")))
}

for(var in c("GG237","GG238","GG239","GG240","GG241","GG242")){
  eval(parse(text=paste("raw_dat$temp_", var, " <- comm_18mplus_funct(var=raw_dat$", var, ")", sep="")))
}

for(var in c("LL174","LL175","LL176","LL177","LL178","LL179","LL180")){
  eval(parse(text=paste("raw_dat$temp_", var, " <- comm_18mplus_funct(var=raw_dat$", var, ")", sep="")))
}

for(var in c("NN211","NN212","NN213","NN215","NN216","NN217","NN223","NN225")){
  eval(parse(text=paste("raw_dat$temp_", var, " <- comm_8yr_funct(var=raw_dat$", var, ")", sep="")))
}

for(var in c("NN220","NN221","NN222","NN224","NN226")){
  eval(parse(text=paste("raw_dat$temp_", var, " <- comm_8yr_reverse_funct(var=raw_dat$", var, ")", sep="")))
}

dat$neuro_development_stage0<-rowSums(raw_dat[,c("temp_DD351","temp_DD352","temp_DD353","temp_DD354","temp_DD355")], na.rm=TRUE)
dat$neuro_development_stage1<-rowSums(raw_dat[,c("temp_EE403","temp_EE404","temp_EE405")], na.rm=TRUE)
dat$neuro_development_stage2<-rowSums(raw_dat[,c("temp_GG237","temp_GG238","temp_GG239","temp_GG240","temp_GG241","temp_GG242")], na.rm=TRUE)
dat$neuro_development_stage3<-rowSums(raw_dat[,c("temp_LL174","temp_LL175","temp_LL176","temp_LL177","temp_LL178","temp_LL179","temp_LL180")], na.rm=TRUE)
dat$neuro_development_stage4<-rowSums(raw_dat[,c("temp_NN211","temp_NN212","temp_NN213","temp_NN215","temp_NN216","temp_NN217",
                                          "temp_NN223","temp_NN225","temp_NN220","temp_NN221","temp_NN222","temp_NN224","temp_NN226")], na.rm=TRUE)

dat$neuro_development_stage0[apply(raw_dat[,c("temp_DD351","temp_DD352","temp_DD353","temp_DD354","temp_DD355")],1,function(x)all(is.na(x)))] <- NA
dat$neuro_development_stage1[apply(raw_dat[,c("temp_EE403","temp_EE404","temp_EE405")],1,function(x)all(is.na(x)))] <- NA
dat$neuro_development_stage2[apply(raw_dat[,c("temp_GG237","temp_GG238","temp_GG239","temp_GG240","temp_GG241","temp_GG242")],1,function(x)all(is.na(x)))] <- NA
dat$neuro_development_stage3[apply(raw_dat[,c("temp_LL174","temp_LL175","temp_LL176","temp_LL177","temp_LL178","temp_LL179","temp_LL180")],1,function(x)all(is.na(x)))] <- NA
dat$neuro_development_stage4[apply(raw_dat[,c("temp_NN211","temp_NN212","temp_NN213","temp_NN215","temp_NN216","temp_NN217",
                                              "temp_NN223","temp_NN225","temp_NN220","temp_NN221","temp_NN222","temp_NN224","temp_NN226")],1,function(x)all(is.na(x)))] <- NA


dat$neuro_development_stage0_zscore<-scale(dat$neuro_development_stage0)
dat$neuro_development_stage1_zscore<-scale(dat$neuro_development_stage1)
dat$neuro_development_stage2_zscore<-scale(dat$neuro_development_stage2)
dat$neuro_development_stage3_zscore<-scale(dat$neuro_development_stage3)
dat$neuro_development_stage4_zscore<-scale(dat$neuro_development_stage4)


################################################################
####            child's immunological outcomes              ####
################################################################

# Wheeze
dat$immuno_wheeze_stage1_binary<-NA
dat$immuno_wheeze_stage1_binary[raw_dat$EE299=="Yes"]<-1
dat$immuno_wheeze_stage1_binary[raw_dat$EE299=="No"]<-0

dat$immuno_wheeze_stage3_binary<-NA
dat$immuno_wheeze_stage3_binary[raw_dat$LL66=="Yes"]<-1 #yes now
dat$immuno_wheeze_stage3_binary[raw_dat$LL64=="No"]<-0 #not now or in the past

dat$immuno_wheeze_allstages_binary <- NA
dat$immuno_wheeze_allstages_binary[dat$immuno_wheeze_stage1_binary==0|
                                     dat$immuno_wheeze_stage3_binary==0] <-0
dat$immuno_wheeze_allstages_binary[dat$immuno_wheeze_stage1_binary==1|
                                     raw_dat$LL64=="Yes"| #yes has ever (but not now at age 5)
                                     dat$immuno_wheeze_stage3_binary==1] <-1

# maternal report asthma
dat$immuno_asthma_stage0_binary<-NA
dat$immuno_asthma_stage0_binary[raw_dat$DD245=="No"|raw_dat$DD937=="No"]<-0 #has not had ever
dat$immuno_asthma_stage0_binary[raw_dat$DD245=="Yes"|raw_dat$DD937=="Yes"]<-1 #has had at some point

dat$immuno_asthma_stage1_binary<-NA
dat$immuno_asthma_stage1_binary[raw_dat$EE823==1|raw_dat$EE197=="No"]<-0 #has not had ever
dat$immuno_asthma_stage1_binary[raw_dat$EE824==1|raw_dat$EE197=="Yes"]<-1 #yes,now/yes has the problem

dat$immuno_asthma_stage2_binary<-NA
dat$immuno_asthma_stage2_binary[raw_dat$GG69==1]<-0 #has not had ever
dat$immuno_asthma_stage2_binary[raw_dat$GG70==1]<-1 #yes has it now

dat$immuno_asthma_stage3_binary<-NA
dat$immuno_asthma_stage3_binary[raw_dat$LL58=="No"]<-0 #not now or in the past
dat$immuno_asthma_stage3_binary[raw_dat$LL60=="Yes"]<-1 #yes, now

  # available for age 7 if can get access
dat$immuno_asthma_allstages_binary <- NA
dat$immuno_asthma_allstages_binary[dat$immuno_asthma_stage0_binary==0|
                                     dat$immuno_asthma_stage1_binary==0|
                                     dat$immuno_asthma_stage2_binary==0|
                                     dat$immuno_asthma_stage3_binary==0] <-0
dat$immuno_asthma_allstages_binary[dat$immuno_asthma_stage0_binary==1|
                                     dat$immuno_asthma_stage1_binary==1|
                                     dat$immuno_asthma_stage2_binary==1|
                                     raw_dat$LL58=="Yes"|#had it previously
                                     raw_dat$GG71==1|#had it previously
                                     raw_dat$EE825==1|#had it previously
                                     dat$immuno_asthma_stage3_binary==1] <-1

## NA for stage 4

# Eczema-like rash or eczema (atopic eczema or other type of eczema)
dat$immuno_eczema_stage0_binary<-NA
dat$immuno_eczema_stage0_binary[raw_dat$DD247=="No"| raw_dat$DD940=="No"]<-0 #atopic eczema (childhood eczema) never had
dat$immuno_eczema_stage0_binary[raw_dat$DD247=="Yes" | raw_dat$DD940=="Yes"]<-1 #had at some point


dat$immuno_eczema_stage1_binary<-NA
dat$immuno_eczema_stage1_binary[raw_dat$EE827==1|raw_dat$EE199=="No"]<-0 #no
dat$immuno_eczema_stage1_binary[raw_dat$EE828==1|raw_dat$EE199=="Yes"]<-1 #has it now/has the problem

dat$immuno_eczema_stage2_binary<-NA
dat$immuno_eczema_stage2_binary[raw_dat$GG77==1|raw_dat$GG81==1]<-0
dat$immuno_eczema_stage2_binary[raw_dat$GG78==1|raw_dat$GG82==1]<-1

dat$immuno_eczema_allstages_binary <- NA
dat$immuno_eczema_allstages_binary[dat$immuno_eczema_stage0_binary==0|
                                    dat$immuno_eczema_stage1_binary==0|
                                    dat$immuno_eczema_stage2_binary==0] <-0
dat$immuno_eczema_allstages_binary[dat$immuno_eczema_stage0_binary==1|
                                    dat$immuno_eczema_stage1_binary==1|
                                    dat$immuno_eczema_stage2_binary==1|
                                     raw_dat$EE829==1|
                                     raw_dat$GG79==1|raw_dat$GG83==1] <-1

  ## stage 3 and 4 not available for eczema (unless in 7)

# Allergies (pet,pollen,mite/dust, bee/wasp,food)
# pet
  ## NA

#pollen
dat$immuno_allergy_pollen_stage1_binary<-NA # tested
dat$immuno_allergy_pollen_stage1_binary[raw_dat$EE348=="Yes"]<-1
dat$immuno_allergy_pollen_stage1_binary[raw_dat$EE348=="No"]<-0

dat$immuno_allergy_pollen_stage3_binary<-NA
dat$immuno_allergy_pollen_stage3_binary[raw_dat$LL63=="No"|raw_dat$LL61=="No"]<-0
dat$immuno_allergy_pollen_stage3_binary[raw_dat$LL63=="Yes"]<-1 #has now

dat$immuno_allergy_pollen_allstages_binary <- NA
dat$immuno_allergy_pollen_allstages_binary[dat$immuno_allergy_pollen_stage1_binary==0|
                                             dat$immuno_allergy_pollen_stage3_binary==0 ] <- 0
dat$immuno_allergy_pollen_allstages_binary[dat$immuno_allergy_pollen_stage1_binary==1|
                                             dat$immuno_allergy_pollen_stage3_binary==1 |
                                             raw_dat$LL61=="Yes"] <- 1

  ## Is in age 7 Q if can get access (stage 3)
  ## NA for stage 4


#mites (tested)
dat$immuno_allergy_dust_stage1_binary[raw_dat$EE344=="Yes"]<-1
dat$immuno_allergy_dust_stage1_binary[raw_dat$EE344=="No"]<-0

#bee/wasp
  ##NA

#pets (animals)
dat$immuno_allergy_pet_stage1_binary[raw_dat$EE346=="Yes"]<-1
dat$immuno_allergy_pet_stage1_binary[raw_dat$EE346=="No"]<-0


#food
dat$immuno_allergy_food_stage0_binary<-NA
dat$immuno_allergy_food_stage0_binary[raw_dat$DD943=="No"|raw_dat$DD251=="No"]<-0
dat$immuno_allergy_food_stage0_binary[raw_dat$DD943=="Yes"|raw_dat$DD251=="Yes"]<-1
dat$immuno_allergy_food_stage1_binary<-NA
# also raw_dat$EE102==2, but dont have in dataset
dat$immuno_allergy_food_stage1_binary[raw_dat$EE835==1|raw_dat$EE203=="No"]<-0
dat$immuno_allergy_food_stage1_binary[raw_dat$EE836==1|raw_dat$EE203=="Yes"]<-1

dat$immuno_allergy_food_stage2_binary<-NA
dat$immuno_allergy_food_stage2_binary[raw_dat$GG86==1]<-1
dat$immuno_allergy_food_stage2_binary[raw_dat$GG85==1]<-0

dat$immuno_allergy_food_allstages_binary <- NA
dat$immuno_allergy_food_allstages_binary[dat$immuno_allergy_food_stage0_binary==0|
                                             dat$immuno_allergy_food_stage1_binary==0 |
                                           dat$immuno_allergy_food_stage2_binary==0] <- 0
dat$immuno_allergy_food_allstages_binary[dat$immuno_allergy_food_stage0_binary==1|
                                           dat$immuno_allergy_food_stage1_binary==1 |
                                           dat$immuno_allergy_food_stage2_binary==1|
                                          raw_dat$EE837==1 | raw_dat$GG87==1] <- 1 #EE837 and GG87 is had previously

## NA for stage 4

#any allergy stage 1
dat$immuno_allergy_any_stage1_binary <- NA
dat$immuno_allergy_any_stage1_binary[which(dat$immuno_allergy_food_stage1_binary==0|dat$immuno_allergy_pollen_stage1_binary==0|dat$immuno_allergy_dust_stage1_binary==0|dat$immuno_allergy_pet_stage1_binary==0)] <-0
dat$immuno_allergy_any_stage1_binary[which(dat$immuno_allergy_food_stage1_binary==1| dat$immuno_allergy_pollen_stage1_binary==1|dat$immuno_allergy_dust_stage1_binary==1|dat$immuno_allergy_pet_stage1_binary==1)] <-1

## if did any allergy for other stages individually it would just be areplication of the single allergy recorded at each timepoint.
## but that's fine as long as we know that...
dat$immuno_allergy_any_stage0_binary <-dat$immuno_allergy_food_stage0_binary
dat$immuno_allergy_any_stage2_binary <-dat$immuno_allergy_food_stage2_binary
dat$immuno_allergy_any_stage3_binary <-dat$immuno_allergy_pollen_stage3_binary

dat$immuno_allergy_any_allstages_binary <- rowSums(dat[,c("immuno_allergy_food_allstages_binary","immuno_allergy_pollen_allstages_binary","immuno_allergy_dust_stage1_binary","immuno_allergy_pet_stage1_binary")],na.rm=T)
dat$immuno_allergy_any_allstages_binary[dat$immuno_allergy_any_allstages_binary>0]<-1
dat$immuno_allergy_any_allstages_binary[which(
  apply(dat[,c("immuno_allergy_food_allstages_binary","immuno_allergy_pollen_allstages_binary","immuno_allergy_dust_stage1_binary","immuno_allergy_pet_stage1_binary")],1,function(x) all(is.na(x)))
  )] <- NA


###################################################################
################      Covariates                      #############
###################################################################

# parity
dat$covs_parity_mother_binary<-NA
dat$covs_parity_mother_binary[raw_dat$PARITET_5=="0 (primiparous)"]<-0
dat$covs_parity_mother_binary[raw_dat$PARITET_5%in%c("1","2","3","4 or more")]<-1

# ethnicity/country of Birth
  ## country of birth: FODELAND_KAT_NOR_GBD (but not available in dataset)

# Maternal and paternal age at conception and/or delivery
raw_dat$MORS_ALDER <- as.character(raw_dat$MORS_ALDER)
raw_dat$MORS_ALDER[raw_dat$MORS_ALDER=="<17"]<-"16"
raw_dat$MORS_ALDER[raw_dat$MORS_ALDER==">45"]<-"46"
dat$covs_age_mother_delivery <- as.numeric(raw_dat$MORS_ALDER)

raw_dat$FARS_ALDER <- as.character(raw_dat$FARS_ALDER)
raw_dat$FARS_ALDER[raw_dat$FARS_ALDER=="< 18"]<-"17"
raw_dat$FARS_ALDER[raw_dat$FARS_ALDER=="> 59"]<-"60"
dat$covs_age_father_delivery <- as.numeric(raw_dat$FARS_ALDER)

# Maternal and paternal education
  ## on a different scale to alspac and bib
  ##mother and mreported are on the same scale
dat$covs_edu_mother<-NA
dat$covs_edu_mother[raw_dat$AA1124=="9-year elementary education"]<-0 # 9-year secondary school
dat$covs_edu_mother[raw_dat$AA1124=="Further education - vocational"|raw_dat$AA1124=="Further education 1-2 years"|raw_dat$AA1124=="Further education 3 years - (general studies, sixth form)"]<-1 #A level or vocational
dat$covs_edu_mother[raw_dat$AA1124=="Higher education (university/college), over 4 years"|raw_dat$AA1124=="Higher education (university/college), up to and including 4 years"]<-2 # university

dat$covs_edu_mother_highestlowest_binary <- NA
dat$covs_edu_mother_highestlowest_binary[dat$covs_edu_mother==0] <- 1
dat$covs_edu_mother_highestlowest_binary[dat$covs_edu_mother%in%c(1,2)] <- 0

# Mother report for father - used for N, overwritten if self-reported
dat$covs_edu_father_mreport<-NA
dat$covs_edu_father_mreport[raw_dat$AA1126=="9-year elementary education"]<-0 # 9-year secondary school
dat$covs_edu_father_mreport[raw_dat$AA1126=="Further education - vocational"|raw_dat$AA1126=="Further education 1-2 years"|raw_dat$AA1126=="Further education 3 years - (general studies, sixth form)"]<-1 # A level or vocational
dat$covs_edu_father_mreport[raw_dat$AA1126=="Higher education (university/college), over 4 years"|raw_dat$AA1126=="Higher education (university/college), up to and including 4 years"]<-2 # university

dat$covs_edu_father_selfreport<-NA
dat$covs_edu_father_selfreport[raw_dat$FF16=="9-year elementary education"]<-0  # secondary school
dat$covs_edu_father_selfreport[raw_dat$FF16=="Further education - vocational"|raw_dat$FF16=="Further education 1-2 years"|raw_dat$FF16=="Further education 3 years - (general studies, sixth form)"]<-1 # A level or vocational
dat$covs_edu_father_selfreport[raw_dat$FF16=="Higher education (university/college), over 4 years"|raw_dat$FF16=="Higher education (university/college), up to and including 4 years"]<-2 # university

dat$covs_edu_father_anyreport <- dat$covs_edu_father_selfreport
dat$covs_edu_father_anyreport[is.na(dat$covs_edu_father_selfreport)] <- dat$covs_edu_father_mreport[is.na(dat$covs_edu_father_selfreport)]

dat$covs_edu_father_highestlowest_binary <- NA
dat$covs_edu_father_highestlowest_binary[dat$covs_edu_father_anyreport%in%c(1,2)] <- 0
dat$covs_edu_father_highestlowest_binary[dat$covs_edu_father_anyreport==0] <- 1

dat$covs_edu_father_highestlowest_binary_anyreport <- dat$covs_edu_father_highestlowest_binary

dat$covs_edu_father_mreport<-NULL
dat$covs_edu_father_selfreport<-NULL


# Maternal and paternal occupation
  ## coded differently to alspac, but in line with lifecycle for moba
##NOT USED AS CAN'T BE HARMONISED WITH ALSPAC AND MCS, AND BIB DOESN'T HAVE DATA
# dat$covs_occup_mother<-NA
# dat$covs_occup_mother[raw_dat$AA1144==1|raw_dat$AA1146==1]<-"employed"
# dat$covs_occup_mother[raw_dat$AA1148==1|raw_dat$AA1150==1]<-"self-employed"
# dat$covs_occup_mother[raw_dat$AA1140==1]<-"unemployed"
# dat$covs_occup_mother[raw_dat$AA1132==1|raw_dat$AA1136==1]<-"student/apprentice"
# dat$covs_occup_mother[raw_dat$AA1134==1]<-"domestic tasks (housewife etc)"
# dat$covs_occup_mother[raw_dat$AA1142==1|raw_dat$AA1152==1]<-"inactive/other(receiving benefits or pension)"
# dat$covs_occup_mother[raw_dat$AA1138==1]<-"military"
# 
# ## here 'lowest is unemployed, in alspac it is sem-skilled or unskilled
# dat$covs_occup_mother_highestlowest_binary <- NA
# dat$covs_occup_mother_highestlowest_binary[dat$covs_occup_mother=="unemployed"] <- 0
# dat$covs_occup_mother_highestlowest_binary[dat$covs_occup_mother=="employed"] <- 1
#                       
# # hadn't included these father vars in restricted father sample merged, so therefore merged from raw fathers file
# dat$covs_occup_father<-NA
# dat$covs_occup_father[fathers_raw$FF26==1|fathers_raw$FF27==1]<-"employed"
# dat$covs_occup_father[fathers_raw$FF28==1|fathers_raw$FF29==1]<-"self-employed"
# dat$covs_occup_father[raw_dat$FF24==1]<-"unemployed"
# dat$covs_occup_father[fathers_raw$FF20==1|fathers_raw$FF22==1]<-"student/apprentice"
# dat$covs_occup_father[fathers_raw$FF21==1]<-"domestic tasks (housewife etc)"
# dat$covs_occup_father[fathers_raw$FF25==1|fathers_raw$FF30==1]<-"inactive/other(receiving benefits or pension)"
# dat$covs_occup_father[raw_dat$FF23==1]<-"military"
#  
# #Script if want to make but not included in final dat                     
# #dat$covs_occup_father_mreport<-NA
# #dat$covs_occup_father_mreport[raw_dat$AA1145==1|raw_dat$AA1147==1]<-"employed"
# #dat$covs_occup_father_mreport[raw_dat$AA1149==1|raw_dat$AA1151==1]<-"self-employed"
# #dat$covs_occup_father_mreport[raw_dat$AA1141==1]<-"unemployed"
# #dat$covs_occup_father_mreport[raw_dat$AA1133==1|raw_dat$AA1137]<-"student/apprentice"
# #dat$covs_occup_father_mreport[raw_dat$AA1135==1]<-"domestic tasks (housewife etc)"
# #dat$covs_occup_father_mreport[raw_dat$AA1143==1|raw_dat$AA1153==1]<-"inactive/other(receiving benefits or pension)" #includes rehab/disabled, other
# #dat$covs_occup_father_mreport[raw_dat$AA1139==1]<-"military"
# #dat$covs_occup_father_mreport<-NULL
# 
# ## here 'lowest is unemployed, in alspac it is sem-skilled or unskilled
# dat$covs_occup_father_highestlowest_binary <- NA
# dat$covs_occup_father_highestlowest_binary[dat$covs_occup_father=="unemployed"] <- 0
# dat$covs_occup_father_highestlowest_binary[dat$covs_occup_father=="employed"] <- 1

# Family relationships
dat$covs_married_father<-NA
dat$covs_married_father[raw_dat$FF15%in%c("Co-habitant","Single","Divorced/separated","Widower","Other")]<-0
dat$covs_married_father[raw_dat$FF15=="Married"]<-1

dat$covs_married_mother<-NA
dat$covs_married_mother[raw_dat$CC1178=="Other"|raw_dat$CC1178=="Co-habitant"|raw_dat$CC1178=="Single"|raw_dat$CC1178=="Divorced/separated"|raw_dat$CC1178=="Widow"]<-0
dat$covs_married_mother[raw_dat$CC1178=="Married"]<-1

# cohabiting
dat$covs_partner_lives_with_mother_prenatal<-NA
dat$covs_partner_lives_with_mother_prenatal[raw_dat$AA1293==1]<-1
dat$covs_partner_lives_with_mother_prenatal[raw_dat$AA1294==1|raw_dat$AA1295==1|raw_dat$AA1296==1|raw_dat$AA1297==1|raw_dat$AA1298==1]<-0

# parents living with child postnatally
dat$covs_partner_lives_with_mother_stage1<-NA
dat$covs_partner_lives_with_mother_stage1[raw_dat$EE488=="Yes"]<-1
dat$covs_partner_lives_with_mother_stage1[raw_dat$EE488=="No"]<-0

dat$covs_partner_lives_with_mother_stage2<-NA
dat$covs_partner_lives_with_mother_stage2[raw_dat$GG383=="Yes"]<-1
dat$covs_partner_lives_with_mother_stage2[raw_dat$GG383=="No"]<-0

dat$covs_partner_lives_with_mother_stage3<-NA
dat$covs_partner_lives_with_mother_stage3[raw_dat$LL26=="Yes"]<-1
dat$covs_partner_lives_with_mother_stage3[raw_dat$LL26=="No"|raw_dat$LL26=="Has never lived with the childs father"]<-0
  
dat$covs_partner_lives_with_mother_stage4<-NA
dat$covs_partner_lives_with_mother_stage4[raw_dat$NN278=="Yes"]<-1
dat$covs_partner_lives_with_mother_stage4[raw_dat$NN278=="No"|raw_dat$NN278=="No, we have moved apart"|raw_dat$NN278=="No, I have never lived with the child's father"]<-0

# biological father
  ## NA in dataset

######################
## Parents' health  ##
######################

# dietary supplements during pregnancy
dat$covs_supplements_mother_binary <-NA
dat$covs_supplements_mother_binary[which(dat$maternal_participation==1)]<-0
dat$covs_supplements_mother_binary[raw_dat$AA943==1|raw_dat$AA944==1|raw_dat$AA945==1]<-1

# illegal drug use during pregnancy
dat$covs_drugs_mother_binary<-NA
dat$covs_drugs_mother_binary[which(raw_dat$AA1436==1&raw_dat$AA1440==1&raw_dat$AA1444==1&raw_dat$AA1448==1)]<-0
dat$covs_drugs_mother_binary[which(raw_dat$AA1439==1|raw_dat$AA1443==1|raw_dat$AA1447==1|raw_dat$AA1451==1)]<-1

# drug use (not including cannabis)
dat$covs_drugs_mother_ever_binary<-NA
dat$covs_drugs_mother_ever_binary[which(raw_dat$AA1436==1& raw_dat$AA1440==1&raw_dat$AA1444==1&raw_dat$AA1448==1)]<-0
dat$covs_drugs_mother_ever_binary[which(raw_dat$AA1437==1|raw_dat$AA1438==1|raw_dat$AA1439==1|
                                     raw_dat$AA1441==1| raw_dat$AA1442==1|raw_dat$AA1443==1|
                                     raw_dat$AA1445==1|raw_dat$AA1446==1|raw_dat$AA1447==1|
                                     raw_dat$AA1449==1|raw_dat$AA1450==1|raw_dat$AA1451==1)]<-1


dat$covs_drugs_father_ever_binary<-NA
dat$covs_drugs_father_ever_binary[which(raw_dat$FF226==1 & raw_dat$FF230==1 & raw_dat$FF234==1 & raw_dat$FF553==1)]<-0 
dat$covs_drugs_father_ever_binary[which(raw_dat$FF227==1|raw_dat$FF228==1|raw_dat$FF229==1|
                                    raw_dat$FF231==1|raw_dat$FF232==1|raw_dat$FF233==1|
                                    raw_dat$FF235==1|raw_dat$FF236==1|raw_dat$FF237==1|
                                    raw_dat$FF554==1|raw_dat$FF555==1|raw_dat$FF556==1|
                                    raw_dat$FF462==1|raw_dat$FF463==1|raw_dat$FF464==1)]<-1

# asthma
dat$covs_asthma_mother_binary<-NA
dat$covs_asthma_mother_binary[which(dat$maternal_participation==1)]<-0
dat$covs_asthma_mother_binary[raw_dat$AA419==1 |raw_dat$AA420==1]<-1


dat$covs_asthma_father_binary<-NA
dat$covs_asthma_father_binary[which(dat$paternal_participation==1)]<-0
dat$covs_asthma_father_binary[raw_dat$FF122==1]<-1

# eczema
dat$covs_eczema_mother_binary<-NA
dat$covs_eczema_mother_binary[dat$maternal_participation==1]<-0
dat$covs_eczema_mother_binary[raw_dat$AA482==1|raw_dat$AA483==1]<-1
dat$covs_eczema_mother_binary[which(dat$covs_eczema_mother_binary!=1)]<-0

dat$covs_eczema_father_binary<-NA
dat$covs_eczema_father_binary[which(dat$paternal_participation==1)]<-0
dat$covs_eczema_father_binary[raw_dat$FF131==1]<-1

# allergy
dat$covs_allergy_mother_binary<-NA
dat$covs_allergy_mother_binary[which(dat$maternal_participation==1)]<-0
dat$covs_allergy_mother_binary[raw_dat$AA428==1|raw_dat$AA429==1|raw_dat$AA437==1|raw_dat$AA438==1|raw_dat$AA446==1 |raw_dat$AA447==1]<-1

dat$covs_allergy_father_binary<-NA
dat$covs_allergy_father_binary[which(dat$paternal_participation==1)]<-0
dat$covs_allergy_father_binary[raw_dat$FF116==1]<-1

# height 
dat$covs_height_mother<-raw_dat$AA87 
dat$covs_height_mother[dat$covs_height_mother<quantile(dat$covs_height_mother,na.rm=T,probs=0.001)]<-NA

dat$covs_height_father<-raw_dat$FF333
dat$covs_height_father[dat$covs_height_father<quantile(dat$covs_height_father,na.rm=T,probs=0.001)]<-NA

covs_height_father_mreport<-raw_dat$AA88
covs_height_father_mreport[covs_height_father_mreport<quantile(covs_height_father_mreport,na.rm=T,probs=0.001)]<-NA

dat$covs_height_father[is.na(dat$covs_height_father)==T]<-covs_height_father_mreport[is.na(dat$covs_height_father)==T]

rm(covs_height_father_mreport)

# weight
dat$covs_weight_mother<-raw_dat$AA85 
dat$covs_weight_mother[dat$covs_weight_mother<quantile(dat$covs_weight_mother,na.rm=T,probs=0.001)|dat$covs_weight_mother>quantile(dat$covs_weight_mother,na.rm=T,probs=0.999)]<-NA

dat$covs_weight_father<-raw_dat$FF334
dat$covs_weight_father[dat$covs_weight_father<quantile(dat$covs_weight_father,na.rm=T,probs=0.001)|dat$covs_weight_father>quantile(dat$covs_weight_father,na.rm=T,probs=0.999)]<-NA

covs_weight_father_mreport<-raw_dat$AA89
covs_weight_father_mreport[covs_weight_father_mreport<quantile(covs_weight_father_mreport,na.rm=T,probs=0.001)|covs_weight_father_mreport>quantile(covs_weight_father_mreport,na.rm=T,probs=0.999)]<-NA

dat$covs_weight_father[is.na(dat$covs_weight_father)==T]<-covs_weight_father_mreport[is.na(dat$covs_weight_father)==T]

rm(covs_weight_father_mreport)


# bmi
dat$covs_bmi_mother<-raw_dat$KMI_FOER
covs_bmi_mother <- dat$covs_weight_mother / (dat$covs_height_mother/100)^2
dat$covs_bmi_mother[is.na(dat$covs_bmi_mother)==T]<-covs_bmi_mother[is.na(dat$covs_bmi_mother)==T]
dat$covs_bmi_mother[dat$covs_bmi_mother>quantile(dat$covs_bmi_mother,na.rm=T,probs=0.999)]<-NA
dat$covs_bmi_mother_zscore<-scale(dat$covs_bmi_mother)

dat$covs_bmi_father<-dat$covs_weight_father/(dat$covs_height_father/100)^2
dat$covs_bmi_father[dat$covs_bmi_father>quantile(dat$covs_bmi_father,na.rm=T,probs=0.999)]<-NA
dat$covs_bmi_father_zscore<-scale(dat$covs_bmi_father)

#######################################
#####    Pregnancy complications  #####
#######################################
# glycosuria, existing diabetes or gestational diabetes Vs none of these
dat$covs_glycosuria_binary<-NA
dat$covs_glycosuria_binary[which(dat$maternal_participation==1)]<-0
dat$covs_glycosuria_binary[raw_dat$AA509==1 |raw_dat$AA510==1 | raw_dat$AA518==1 |raw_dat$AA519==1]<-1

######################
##  Child's health  ##
######################

# gestational age
dat$covs_gestage<-raw_dat$SVLEN

# preterm birth
dat$covs_preterm_binary<-NA
dat$covs_preterm_binary[dat$covs_gestage<37]<-1
dat$covs_preterm_binary[dat$covs_gestage>=37]<-0

# breastfeeding (duration)
#  these also aren't available in Alex's data NA

# alcohol, caffeine and smoking (include passive smoking) (binary) 
  # child's own alcohol use NA
  # child's own caffeine use NA

dat$covs_passivesmk_child_stage1_binary<-NA
dat$covs_passivesmk_child_stage1_binary[raw_dat$EE507=="No"& raw_dat$DD744=="No"]<-0
dat$covs_passivesmk_child_stage1_binary[raw_dat$EE507=="Yes, every day"|raw_dat$EE507=="Yes, several times a week"|raw_dat$EE507=="Yes, sometimes"|raw_dat$DD744=="Yes, every day"|raw_dat$DD744=="Yes, several times a week"|raw_dat$DD744=="Yes, sometimes"]<-1

dat$covs_passivesmk_child_stage2_binary<-NA
dat$covs_passivesmk_child_stage2_binary[raw_dat$GG388=="No"]<-0
dat$covs_passivesmk_child_stage2_binary[raw_dat$GG388=="Yes, every day"|raw_dat$GG388=="Yes, several times a week"|raw_dat$GG388=="Yes, sometimes"]<-1


# Age variables: 
dat$stage1_timepoint_bmi <- as.numeric(dat$stage1_timepoint_bmi)
dat$stage1_timepoint_height <- as.numeric(dat$stage1_timepoint_height)
dat$stage1_timepoint_weight <- as.numeric(dat$stage1_timepoint_weight)
dat$stage1_timepoint_headcirc <- as.numeric(dat$stage1_timepoint_headcirc)


####################################
##      Withdrawal of Consent     ##
####################################

library(haven)
sv_info<-read_sav("N:/durable/data/MoBaPhenoData/PDB2306_MoBa_v12/SPSS/PDB2306_SV_INFO_V12.sav")

dat<- dat[dat$PREG_ID_2306 %in% sv_info$PREG_ID_2306,]

###################
##   Save file   ##
###################

saveRDS(dat,"N:/durable/projects/EPoCH/moba_pheno.rds")


###################################################################################
###############                    Clean 98k data                   ###############
###################################################################################

#Merge on PREG_ID and BARN_NR, and IID for genetic data, with the linkage file as your start point 
## Can start  with a phenotype file instead if you don't require everyone in your sample to have genotype data)
###################################################################################
#Read in phenotype data
###################################################################################
#rm(list=ls())

# dat<- readRDS("N:/durable/projects/EPoCH/moba_pheno.rds")
# 
# ## Read in pheno datasets you want to use e.g. 
# dataset <- dat
# 
# # merge in parity, child sex and parents age at birth (from birth registry data)
# setwd ("N:/durable/data/MoBaPhenoData/PDB2306_MoBa_v12/Statafiles")
# mbrn <- read_dta("PDB2306_MBRN_541_v12.dta")
# mbrn <- mbrn[c("PREG_ID_2306", "KJONN", "PARITET_5", "MORS_ALDER", "FAAR", "DODKAT_G", "FARS_ALDER")]
# dataset <- merge (dataset, mbrn, by="PREG_ID_2306", all=T)
# #The number increases because some of these pregnancies are multiple births (listed in the birth registry twice)
# 
# ###################################################################################
# # Link with genetic ID
# ###################################################################################
# 
# #Read in the linkage file
# link <- read.table("N:/durable/data/genetic/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc-cov.txt", header=T, sep="\t")
# 
# #merge FID from the fam file into the linker file (will use later)
# fam <- read.table("N:/durable/data/genetic/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc.fam", header=F)
# colnames(fam) <- c("FID_fam", "IID","FatherID", "MotherID", "sex", "Phenotype" )
# 
# link_fam <- merge(link, fam, by="IID",  all.x=T)
# 
# 
# ########################################################################
# ##subset to a list of unrelated trios for all analysis 
# ##########################################################################
# #GENOTYPE README: 
# #There are significant genotyping plate, genotyping batch, and imputation batch effects. 
# #Therefore, we highly recommend including these as covariates in all analyses.
# #The ID_2306 column contains the F_ID_2306 for fathers, the M_ID_2306 for mothers, and the PREG_ID_BARN_NR for children (created using the command paste(child$PREG_ID, child$BARN_NR, sep="_") in R). 
# 
# #So because mothers and fathers can have multiple children in MoBa...
# #I will match on child genotype, because that will help me identify the PREG_ID that links mum and dad
# 
# ##Children
# link_child <- subset(link_fam, link_fam$Role=="Child")
# colnames(link_child) <- paste(colnames(link_child), "CHILD", sep="_")
# data_child <- dat #[c("PREG_ID_2306", "BARN_NR")]
# #Now link to the genetic IDs
# data_child$PREG_ID_BARN_NR <- paste(data_child$PREG_ID, data_child$BARN_NR, sep="_")
# gen_child <- merge(data_child, link_child, by.x="PREG_ID_BARN_NR", by.y="ID_2306_CHILD", all.x=T)
# #Count how many genetic IDs there are 
# table(is.na(gen_child$SENTRIXID))
# 
# ## Mothers
# link_mothers <- subset(link_fam, link_fam$Role=="Mother")
# colnames(link_mothers) <- paste(colnames(link_mothers), "MUM", sep="_")
# #Read in the mother ID
# setwd ("N:/durable/data/MoBaPhenoData/PDB2306_MoBa_v12/Statafiles")
# id_vars <- read_dta("PDB2306_SV_INFO_v12.dta")
# m_id <- id_vars[c("PREG_ID_2306", "M_ID_2306")]
# #merge in the dataset that contains mother ID
# phen_mum <- dat[c("PREG_ID_2306", "BARN_NR")]
# data_mothers <- merge(phen_mum, m_id, by="PREG_ID_2306", all.x=T)
# #Now link to the genetic IDs
# gen_mothers <- merge(data_mothers, link_mothers, by.x="M_ID_2306", by.y="ID_2306_MUM", all.x=T)
# #Count how many genetic IDs there are 
# table(is.na(gen_mothers$SENTRIXID)) 
# 
# ##Fathers
# link_fathers <- subset(link_fam, link_fam$Role=="Father")
# colnames(link_fathers) <- paste(colnames(link_fathers), "DAD", sep="_")
# #merge in the father ID
# d_id <- id_vars[c("PREG_ID_2306", "F_ID_2306")]
# phen_dad <- dat[c("PREG_ID_2306", "BARN_NR")]
# 
# data_fathers <- merge(phen_dad, d_id, by.x="PREG_ID_2306", all.x=T)
# #Now link to the genetic IDs
# gen_fathers <- merge(data_fathers, link_fathers, by.x="F_ID_2306", by.y="ID_2306_DAD", all.x=T)
# #Count how many genetic IDs there are 
# table(is.na(gen_fathers$SENTRIXID))
# 
# 
# ###################################################################################
# # Merge child, mother and father datasets
# ###################################################################################
# ## Merge datasets
# 
# #merge on pregnancy ID
# 
# dataset_merged <- merge(gen_child, gen_mothers, by=c("PREG_ID_2306","BARN_NR"), all=T) 
# dataset_merged <- merge(dataset_merged, gen_fathers, by=c("PREG_ID_2306","BARN_NR"), all=T)
# 
# #Check for duplicate IDs
# sum(duplicated(na.omit(dataset_merged$SENTRIXID_CHILD)))
# sum(duplicated(na.omit(dataset_merged$SENTRIXID_MUM)))
# sum(duplicated(na.omit(dataset_merged$SENTRIXID_DAD)))
# 
# ###################################################################################
# # Remove the duplicates, based on whether or not the child ID is present
# ###################################################################################
# ## Remove duplicates/related etc
# 
# #First of all, create a genotype variable that indicates whether anyone in the trio has complete genotype data
# dataset_merged$genotype <- ifelse(is.na(dataset_merged$SENTRIXID_CHILD) & is.na(dataset_merged$SENTRIXID_MUM) & is.na(dataset_merged$SENTRIXID_DAD), 0, 1)
# table(dataset_merged$genotype)
# #If any individual in the trio has genetic data, the row should =1 
# #If mum, dad and child all have NA for genotype, then the row should =0
# #Check if this has worked:
# #View(select(dataset_merged, PREG_ID_2306, BARN_NR,  SENTRIXID_MUM, SENTRIXID_DAD, SENTRIXID_CHILD, genotype))
# 
# #Now subset to only those who have some genetic data
# gen <- subset(dataset_merged, dataset_merged$genotype==1)
# 
# #Now arrange the mother ID in order, putting NA for child ID last
# sum(is.na(gen$M_ID_2306)) #2
# sorted <- gen %>%
#   arrange(M_ID_2306, is.na(SENTRIXID_CHILD)) %>%
# add_count(SENTRIXID_DAD)  
# #Check that has ordered them correctly (when M_ID is duplicated then the complete child ID should come first and NA should come second (if there is an NA))
# #View(select(sorted, PREG_ID_2306, BARN_NR, FAAR, M_ID_2306, SENTRIXID_MUM, SENTRIXID_CHILD, n))
# 
# #Now keep only the first occurrence of each mother 
# #(NA are listed last, so this should keep the preg which has child genetic data if there is one)
# sorted <- gen %>%
#   arrange(M_ID_2306, is.na(SENTRIXID_CHILD)) %>%
#   distinct(M_ID_2306, .keep_all = TRUE)
# #Check if this has worked
# #View(select(sorted, PREG_ID_2306, BARN_NR, FAAR, M_ID_2306, SENTRIXID_MUM, SENTRIXID_CHILD))
# 
# #check for duplicates
# sum(duplicated(na.omit(sorted$SENTRIXID_CHILD))) #0
# sum(duplicated(na.omit(sorted$SENTRIXID_MUM))) #0
# sum(duplicated(na.omit(sorted$SENTRIXID_DAD))) #61 - fathers who have children with more than one MoBa mother (this is actually 94 now)
# 
# ##Check that the only children we removed were twins or sibs
# #First, get the number of children with genetic ID that share a mum
# child <- subset(gen, complete.cases(gen$SENTRIXID_CHILD))
# sum(duplicated(na.omit(child$M_ID_2306))) #81846? duplicated mothers among kids with genetic ID (this is now 10422)
# #Check if this is the same number that we removed
# check <- subset(sorted, complete.cases(sorted$SENTRIXID_CHILD))
# rm(child, check)
# 
# #Now make NA one of each instance of duplicated father
# #We want to prioritise dads that have a corresponding mother or child genetic ID available
# #So make a variable that counts whether or not these are missing
# sorted$gen_count <- rowSums(apply(is.na(sorted[c("SENTRIXID_MUM", "SENTRIXID_CHILD", "SENTRIXID_DAD")]), 2, as.numeric))
# table(sorted$gen_count)
# 
# #Order the father genetic ID by whether or not mother ID /Child ID are available
# sorted2 <- sorted %>%
#   arrange(SENTRIXID_DAD, gen_count)  %>%
#   add_count(SENTRIXID_DAD)  
# #check that is ordered by number of genotypes as well as SENTRIXID_DAD
# #for duplicated dads (find using the n column), the row with more complete mum and child ID should come first
# #View(select(sorted2, PREG_ID_2306, BARN_NR, SENTRIXID_DAD, SENTRIXID_MUM, SENTRIXID_CHILD, gen_count, n))
# 
# #First, for those duplicate Dads, make the children NA
# #Because they will be a half-sib of another MoBa child 
# #Prioritise keeping the first occurence as they are most likely to have gentoype data
# #We are not doing this for the whole family as mothers are unrelated 
# sum(is.na(sorted2$SENTRIXID_CHILD))
# sorted2$SENTRIXID_CHILD[duplicated(na.omit(sorted2$SENTRIXID_DAD))] <- NA
# sum(is.na(sorted2$SENTRIXID_CHILD))
# 
# #Now make duplicated Dad ID other than the first occurence an NA
# sum(is.na(sorted2$SENTRIXID_DAD))
# sorted2$SENTRIXID_DAD[duplicated(sorted2$SENTRIXID_DAD)] <- NA
# sum(is.na(sorted2$SENTRIXID_DAD))
# #check that this kept the Dad ID that has complete child/mum IDs (check n=2)
# #View(select(sorted2, PREG_ID_2306, BARN_NR, SENTRIXID_DAD, SENTRIXID_MUM, SENTRIXID_CHILD, gen_count, n))
# 
# #check for duplicates
# sum(duplicated(na.omit(sorted2$SENTRIXID_CHILD))) #0
# sum(duplicated(na.omit(sorted2$SENTRIXID_MUM))) #0
# sum(duplicated(na.omit(sorted2$SENTRIXID_DAD))) #0
# 
# #To check
# sum(!is.na(sorted2$SENTRIXID_CHILD))
# sum(!is.na(sorted2$SENTRIXID_MUM))
# sum(!is.na(sorted2$SENTRIXID_DAD))
# 
# ###################################################################################
# # Remove families that are related across pregnancies
# ###################################################################################
# #The FID from the fam file ('FID_fam') is NOT unique for each MoBa trio + sibs.
# #It is shared by wider family structures e.g. sibships between parents in MoBa
# #So I will only keep one of each pregnancy based on number genotyped
# 
# #check that FID is the same across families
# #View(select(sorted2, PREG_ID_2306, BARN_NR, FID_fam_CHILD, FID_fam_MUM, FID_fam_DAD))
# sum(is.na(sorted2$FID_fam_MUM))
# 
# #Create an FID column by sticking child, mother and father together
# sorted2$FID_fam <- ifelse(is.na(sorted2$FID_fam_MUM), sorted2$FID_fam_CHILD, sorted2$FID_fam_MUM)
# sorted2$FID_fam <- ifelse(is.na(sorted2$FID_fam), sorted2$FID_fam_DAD, sorted2$FID_fam)
# sum(is.na(sorted2$FID_fam))
# 
# #order by FID_fam by the amount of genotyped individuals in the trio
# sum(is.na(sorted2$FID_fam)) #2
# sorted_fam <- sorted2 %>%
#   arrange(FID_fam, is.na(gen_count))
# #Check that has ordered them correctly 
# #View(select(sorted_fam, PREG_ID_2306, BARN_NR, FID_fam, gen_count, SENTRIXID_MUM, SENTRIXID_CHILD, SENTRIXID_DAD))
# 
# #Now keep only the first occurence of each FID 
# #(NA are listed last, so this should keep the preg which has max genetic data)
# sorted_fam <- sorted2 %>%
#   arrange(FID_fam, is.na(gen_count)) %>%
#   distinct(FID_fam, .keep_all = TRUE) 
# 
# #check for duplicates
# sum(duplicated(na.omit(sorted_fam$FID_fam))) #0
# 
# #Now we have a dataset that contains only unrelated trios ready for analysis
# 
# 
# #####################################
# # Write to file
# #####################################
# 
# #save the final dataset
# write.csv(dataset_merged, "N:/durable/projects/EPoCH/phenotype_98kmerge_2023.csv", quote=FALSE, row.names=FALSE)
# 
# saveRDS(dataset_merged,"N:/durable/projects/EPoCH/phenotype_98kmerge_2023.rds")


###################################################################################
####################                  PRS                     #####################
###################################################################################

# NOTE: I (Gemma) am not able to reproduce any of the following commented out section because I don't have access to the .profile files Kayleigh mentions below.

## Read in PRS 

# #The separate .profile file contains mothers, partners and child data. Just need to separate them after reading in.
# 
# #Read in PRS 
# 
# prs_alcohol<-read.table("M:/p471-kayleighe/genetic_data/PRS/2.98k/20220727_scores_alcohol_offspring.profile", header=TRUE,stringsAsFactors = F)
# 
# prs_caffeine<-read.table("M:/p471-kayleighe/genetic_data/PRS/2.98k/20220727_scores_caffeine_offspring.profile", header=TRUE,stringsAsFactors = F)
# 
# prs_smoking_age_init<-read.table("M:/p471-kayleighe/genetic_data/PRS/2.98k/20220727_scores_smoking_age_init_offspring.profile", header=TRUE,stringsAsFactors = F)
# 
# prs_smoking_cessation<-read.table("M:/p471-kayleighe/genetic_data/PRS/2.98k/20220727_scores_smoking_cessation_offspring.profile", header=TRUE,stringsAsFactors = F)
# 
# prs_smoking_cigs_pd<-read.table("M:/p471-kayleighe/genetic_data/PRS/2.98k/20220727_scores_smoking_cigs_pd_offspring.profile", header=TRUE,stringsAsFactors = F)
# 
# prs_smoking_initiation<-read.table("M:/p471-kayleighe/genetic_data/PRS/2.98k/20220727_scores_smoking_initiation_offspring.profile", header=TRUE,stringsAsFactors = F)
# 
# prs_pa_sedentary<-read.table("M:/p471-kayleighe/genetic_data/PRS/2.98k/20220727_scores_pa_sedentary_offspring.profile", header=TRUE,stringsAsFactors = F)
# 
# prs_pa_overall_activity<-read.table("M:/p471-kayleighe/genetic_data/PRS/2.98k/20220727_scores_pa_overall_activity_offspring.profile", header=TRUE,stringsAsFactors = F)
# 
# 
# ## Rename score column
# 
# prs_alcohol$prs_score_alcohol<-prs_alcohol$SCORE
# ## keep only the columns we want
# prs_alcohol<-prs_alcohol[, which(names(prs_alcohol) %in% c("IID","prs_score_alcohol"))]
# 
# prs_caffeine$prs_score_caffeine<-prs_caffeine$SCORE
# prs_caffeine<-prs_caffeine[, which(names(prs_caffeine) %in% c("IID","prs_score_caffeine"))]
# 
# prs_smoking_age_init$prs_score_smoking_age_init<-prs_smoking_age_init$SCORE
# prs_smoking_age_init<-prs_smoking_age_init[, which(names(prs_smoking_age_init) %in% c("IID","prs_score_smoking_age_init"))]
# 
# prs_smoking_cessation$prs_score_smoking_cessation<-prs_smoking_cessation$SCORE
# prs_smoking_cessation<-prs_smoking_cessation[, which(names(prs_smoking_cessation) %in% c("IID","prs_score_smoking_cessation"))]
# 
# prs_smoking_cigs_pd$prs_score_smoking_cigs_pd<-prs_smoking_cigs_pd$SCORE
# prs_smoking_cigs_pd<-prs_smoking_cigs_pd[, which(names(prs_smoking_cigs_pd) %in% c("IID","prs_score_smoking_cigs_pd"))]
# 
# prs_smoking_initiation$prs_score_smoking_initiation<-prs_smoking_initiation$SCORE
# prs_smoking_initiation<-prs_smoking_initiation[, which(names(prs_smoking_initiation) %in% c("IID","prs_score_smoking_initiation"))]
# 
# prs_pa_sedentary$prs_score_pa_sedentary<-prs_pa_sedentary$SCORE
# prs_pa_sedentary<-prs_pa_sedentary[, which(names(prs_pa_sedentary) %in% c("IID","prs_score_pa_sedentary"))]
# 
# prs_pa_overall_activity$prs_score_pa_overall_activity<-prs_pa_overall_activity$SCORE
# prs_pa_overall_activity<-prs_pa_overall_activity[, which(names(prs_pa_overall_activity) %in% c("IID","prs_score_pa_overall_activity"))]
# 
# 
# ## Split mother, child and father PRS into separate variables:
# 
# #1) Merge PRS files with individual linkage files for mum, dads and children, keeping only the data in the linkage files
# ##### Mother
# # rename first column to merge
# link_mothers$IID<-link_mothers$IID_MUM
# #merge prs with linkage file
# prs_mother<-merge(link_mothers, prs_alcohol, by="IID", all.x=TRUE)
# prs_mother<-merge(prs_mother, prs_caffeine, by="IID", all.x=TRUE)
# prs_mother<-merge(prs_mother, prs_smoking_age_init, by="IID", all.x=TRUE)
# prs_mother<-merge(prs_mother, prs_smoking_cessation, by="IID", all.x=TRUE)
# prs_mother<-merge(prs_mother, prs_smoking_cigs_pd, by="IID", all.x=TRUE)
# prs_mother<-merge(prs_mother, prs_smoking_initiation, by="IID", all.x=TRUE)
# prs_mother<-merge(prs_mother, prs_pa_overall_activity, by="IID", all.x=TRUE)
# prs_mother<-merge(prs_mother, prs_pa_sedentary, by="IID", all.x=TRUE)
# 
# #rename score variables 
# prs_mother$prs_alcohol_mother<-prs_mother$prs_score_alcohol
# prs_mother$prs_caffeine_mother<-prs_mother$prs_score_caffeine
# prs_mother$prs_smoking_age_init_mother<-prs_mother$prs_score_smoking_age_init
# prs_mother$prs_smoking_cessation_mother<-prs_mother$prs_score_smoking_cessation
# prs_mother$prs_smoking_cigs_pd_mother<-prs_mother$prs_score_smoking_cigs_pd
# prs_mother$prs_smoking_initiation_mother<-prs_mother$prs_score_smoking_initiation
# prs_mother$prs_pa_overall_activity_mother<-prs_mother$prs_score_pa_overall_activity
# prs_mother$prs_pa_sedentary_mother<-prs_mother$prs_score_pa_sedentary
# 
# #subset to vars I want
# prs_mother<-subset(prs_mother, select=c("IID", "prs_alcohol_mother","prs_caffeine_mother","prs_smoking_age_init_mother","prs_smoking_cessation_mother","prs_smoking_cigs_pd_mother","prs_smoking_initiation_mother"))
# 
# #save mother prs file
# saveRDS(prs_mother,"M:/p471-kayleighe/epoch_data/prs/prs_mother.rds")
# 
# ##### Child
# # rename first column to merge
# link_child$IID<-link_child$IID_CHILD
# #merge prs with linkage file
# prs_child<-merge(link_child, prs_alcohol, by="IID", all.x=TRUE)
# prs_child<-merge(prs_child, prs_caffeine, by="IID", all.x=TRUE)
# prs_child<-merge(prs_child, prs_smoking_age_init, by="IID", all.x=TRUE)
# prs_child<-merge(prs_child, prs_smoking_cessation, by="IID", all.x=TRUE)
# prs_child<-merge(prs_child, prs_smoking_cigs_pd, by="IID", all.x=TRUE)
# prs_child<-merge(prs_child, prs_smoking_initiation, by="IID", all.x=TRUE)
# prs_child<-merge(prs_child, prs_pa_overall_activity, by="IID", all.x=TRUE)
# prs_child<-merge(prs_child, prs_pa_sedentary, by="IID", all.x=TRUE)
# 
# #rename score variables 
# prs_child$prs_alcohol_child<-prs_child$prs_score_alcohol
# prs_child$prs_caffeine_child<-prs_child$prs_score_caffeine
# prs_child$prs_smoking_age_init_child<-prs_child$prs_score_smoking_age_init
# prs_child$prs_smoking_cessation_child<-prs_child$prs_score_smoking_cessation
# prs_child$prs_smoking_cigs_pd_child<-prs_child$prs_score_smoking_cigs_pd
# prs_child$prs_smoking_initiation_child<-prs_child$prs_score_smoking_initiation
# prs_child$prs_pa_overall_activity<-prs_child$prs_score_score_pa_overall_activity
# prs_child$prs_pa_sedentary<-prs_child$prs_score_pa_sedentary
# 
# #subset to vars I want
# prs_child<-subset(prs_child, select=c("IID", "prs_alcohol_child","prs_caffeine_child","prs_smoking_age_init_child","prs_smoking_cessation_child","prs_smoking_cigs_pd_child","prs_smoking_initiation_child"))
# 
# #save child prs file
# saveRDS(prs_child,"M:/p471-kayleighe/epoch_data/prs/prs_child.rds")
# 
# #2) merge back in to 98k data to keep only the data in the 98k sorted file (eg removes relatedness etc)
# ##### father
# # rename first column to merge
# link_fathers$IID<-link_fathers$IID_DAD
# #merge prs with linkage file
# prs_father<-merge(link_fathers, prs_alcohol, by="IID", all.x=TRUE)
# prs_father<-merge(prs_father, prs_caffeine, by="IID", all.x=TRUE)
# prs_father<-merge(prs_father, prs_smoking_age_init, by="IID", all.x=TRUE)
# prs_father<-merge(prs_father, prs_smoking_cessation, by="IID", all.x=TRUE)
# prs_father<-merge(prs_father, prs_smoking_cigs_pd, by="IID", all.x=TRUE)
# prs_father<-merge(prs_father, prs_smoking_initiation, by="IID", all.x=TRUE)
# prs_father<-merge(prs_father, prs_pa_overall_activity, by="IID", all.x=TRUE)
# prs_father<-merge(prs_father, prs_pa_sedentary, by="IID", all.x=TRUE)
# 
# #rename score variables 
# prs_father$prs_alcohol_father<-prs_father$prs_score_alcohol
# prs_father$prs_caffeine_father<-prs_father$prs_score_caffeine
# prs_father$prs_smoking_age_init_father<-prs_father$prs_score_smoking_age_init
# prs_father$prs_smoking_cessation_father<-prs_father$prs_score_smoking_cessation
# prs_father$prs_smoking_cigs_pd_father<-prs_father$prs_score_smoking_cigs_pd
# prs_father$prs_smoking_initiation_father<-prs_father$prs_score_smoking_initiation
# prs_father$prs_pa_overall_activity<-prs_father$prs_score_pa_overall_activity
# prs_father$prs_pa_sedentary<-prs_father$prs_score_pa_sedentary
# 
# #subset to vars I want
# prs_father<-subset(prs_father, select=c("IID", "prs_alcohol_father","prs_caffeine_father","prs_smoking_age_init_father","prs_smoking_cessation_father","prs_smoking_cigs_pd_father","prs_smoking_initiation_father"))
# 
# #save father prs file
# saveRDS(prs_father,"M:/p471-kayleighe/epoch_data/prs/prs_father.rds")
# # read file if needed
# prs_father<-readRDS("M:/p471-kayleighe/epoch_data/prs/prs_father.rds")
# 
# 
# # 3) Merge PRS and sorted fam
# 
# all_gen <- merge(sorted_fam, prs_child, by.x="IID_CHILD",by.y="IID", all.x=T)
# 
# all_gen <- merge(all_gen, prs_mother, by.x="IID_MUM",by.y="IID", all.x=T)
# 
# all_gen <- merge(all_gen, prs_father, by.x="IID_DAD",by.y="IID", all.x=T)

# Save just gen
#saveRDS(all_gen, "M:/p471-kayleighe/epoch_data/prs/all_gen.rds")

#read if needed:
#all_gen<-readRDS("M:/p471-kayleighe/epoch_data/prs/all_gen.rds")

#4) Merge all genetic data with dat file

# read all_gen
all_gen <- readRDS("N:/durable/projects/EPoCH/all_gen.12.10.2022.rds")
#read pheno file
pheno_dat<- readRDS("N:/durable/projects/EPoCH/moba_pheno.rds")

#merge
all_gen <- all_gen[,grep(names(all_gen),pattern="PREG_ID_2306|BARN_NR|prs_|genot|Plate_|PC1|PC2|PC3|PC4|PC5|PC6|PC7|PC8|PC9|batch_|imputation")]
dat<-merge(pheno_dat, all_gen, by=c("PREG_ID_2306","BARN_NR"), all.x = TRUE,all.y = F)

##save

saveRDS(dat,"N:/durable/projects/EPoCH/pheno_and_gen.rds")

#####################################################
########### RENAME PRS IN LINE WITH OTHER COHORTS ###
#####################################################

prs_vars <- names(dat)[grep(names(dat),pattern="prs")]

dat <- rename(dat,prs_score_child_alcohol = prs_alcohol_child)
dat <- rename(dat,prs_score_child_caffeine = prs_caffeine_child)
dat <- rename(dat,prs_score_child_smoking_age_init = prs_smoking_age_init_child)
dat <- rename(dat,prs_score_child_smoking_cessation = prs_smoking_cessation_child)
dat <- rename(dat,prs_score_child_smoking_cigs_pd = prs_smoking_cigs_pd_child)
dat <- rename(dat,prs_score_child_smoking_initiation = prs_smoking_initiation_child)

dat <- rename(dat,prs_score_mother_alcohol = prs_alcohol_mother)
dat <- rename(dat,prs_score_mother_caffeine = prs_caffeine_mother)
dat <- rename(dat,prs_score_mother_smoking_age_init = prs_smoking_age_init_mother)
dat <- rename(dat,prs_score_mother_smoking_cessation = prs_smoking_cessation_mother)
dat <- rename(dat,prs_score_mother_smoking_cigs_pd = prs_smoking_cigs_pd_mother)
dat <- rename(dat,prs_score_mother_smoking_initiation = prs_smoking_initiation_mother)

dat <- rename(dat,prs_score_father_alcohol = prs_alcohol_father)
dat <- rename(dat,prs_score_father_caffeine = prs_caffeine_father)
dat <- rename(dat,prs_score_father_smoking_age_init = prs_smoking_age_init_father)
dat <- rename(dat,prs_score_father_smoking_cessation = prs_smoking_cessation_father)
dat <- rename(dat,prs_score_father_smoking_cigs_pd = prs_smoking_cigs_pd_father)
dat <- rename(dat,prs_score_father_smoking_initiation = prs_smoking_initiation_father)


#################################################
########    Create Z scores for PRS    ##########
#################################################
prs_vars <- names(dat)[grep(names(dat),pattern="prs")]
scaled_prs <- as.data.frame(apply(dat[,prs_vars],2,scale))
names(scaled_prs) <- paste0(prs_vars,"_zscore")
library(dplyr)
dat <- bind_cols(dat,scaled_prs)


#################################################
########    Rename PCs                 ##########
#################################################
#
dat$childpc1<-dat$PC1_CHILD
dat$childpc2<-dat$PC2_CHILD
dat$childpc3<-dat$PC3_CHILD
dat$childpc4<-dat$PC4_CHILD
dat$childpc5<-dat$PC5_CHILD
dat$childpc6<-dat$PC6_CHILD
dat$childpc7<-dat$PC7_CHILD
dat$childpc8<-dat$PC8_CHILD
dat$childpc9<-dat$PC9_CHILD
dat$childpc10<-dat$PC10_CHILD

dat$mumpc1<-dat$PC1_MUM
dat$mumpc2<-dat$PC2_MUM
dat$mumpc3<-dat$PC3_MUM
dat$mumpc4<-dat$PC4_MUM
dat$mumpc5<-dat$PC5_MUM
dat$mumpc6<-dat$PC6_MUM
dat$mumpc7<-dat$PC7_MUM
dat$mumpc8<-dat$PC8_MUM
dat$mumpc9<-dat$PC9_MUM
dat$mumpc10<-dat$PC10_MUM

dat$dadpc1<-dat$PC1_DAD
dat$dadpc2<-dat$PC2_DAD
dat$dadpc3<-dat$PC3_DAD
dat$dadpc4<-dat$PC4_DAD
dat$dadpc5<-dat$PC5_DAD
dat$dadpc6<-dat$PC6_DAD
dat$dadpc7<-dat$PC7_DAD
dat$dadpc8<-dat$PC8_DAD
dat$dadpc9<-dat$PC9_DAD
dat$dadpc10<-dat$PC10_DAD


dat$PC1_CHILD<-NULL
dat$PC2_CHILD<-NULL
dat$PC3_CHILD<-NULL
dat$PC4_CHILD<-NULL
dat$PC5_CHILD<-NULL
dat$PC6_CHILD<-NULL
dat$PC7_CHILD<-NULL
dat$PC8_CHILD<-NULL
dat$PC9_CHILD<-NULL
dat$PC10_CHILD<-NULL

dat$PC1_MUM<-NULL
dat$PC2_MUM<-NULL
dat$PC3_MUM<-NULL
dat$PC4_MUM<-NULL
dat$PC5_MUM<-NULL
dat$PC6_MUM<-NULL
dat$PC7_MUM<-NULL
dat$PC8_MUM<-NULL
dat$PC9_MUM<-NULL
dat$PC10_MUM<-NULL

dat$PC1_DAD<-NULL
dat$PC2_DAD<-NULL
dat$PC3_DAD<-NULL
dat$PC4_DAD<-NULL
dat$PC5_DAD<-NULL
dat$PC6_DAD<-NULL 
dat$PC7_DAD<-NULL
dat$PC8_DAD<-NULL
dat$PC9_DAD<-NULL
dat$PC10_DAD<-NULL


##save

saveRDS(dat,"N:/durable/projects/EPoCH/pheno_and_gen.rds")


##save summary
CreateTableOne(names(dat)[grep("binary|ordinal|covs_|neuro_|immuno_|anthro_",names(dat))],factorVars = names(dat)[grep("binary|ordinal",names(dat))],data=dat[dat$multiple_pregnancy==0,])->TABLE1
saveRDS(TABLE1,"results/table1_for_dat.rds")

##correlations of PRS and exposures

flattenCorrMatrix <- function(cormat,pmat){
  ut <- upper.tri(cormat)
  data.frame(
    var1 = rownames(cormat)[row(cormat)[ut]],
    var2 = rownames(cormat)[col(cormat)[ut]],
    cor = (cormat)[ut],
    p = pmat[ut]
  )
}

generate_correlations <- function(vars, dat) {
vars<-vars[-grep("zscore",vars)]
df <- dat[,vars]
vars_ordinal <- vars[grep("ordinal",vars)]
for(var in vars_ordinal){
  df[,var]<-as.numeric(df[,var])
}
df_cor <- Hmisc::rcorr(as.matrix(df))
flattenCorrMatrix(df_cor$r, df_cor$P)
}


smoking_vars <- names(dat)[grep(names(dat),pattern="smoking_m|smoking_f|prs_score_mother_smoking|prs_score_father_smoking")]
alcohol_vars <- names(dat)[grep(names(dat),pattern="alcohol_m|alcohol_f|prs_score_mother_alcohol|prs_score_father_alcohol")]
caffeine_vars <- names(dat)[grep(names(dat),pattern="caffeine_m|caffeine_f|prs_score_mother_caffeine|prs_score_father_caffeine")]

smoking_cor <- generate_correlations(smoking_vars,dat)
alcohol_cor <- generate_correlations(alcohol_vars,dat)
caffeine_cor <- generate_correlations(caffeine_vars,dat)

saveRDS(smoking_cor,"results/smoking_correlations.rds")
saveRDS(alcohol_cor,"results/alcohol_correlations.rds")
saveRDS(caffeine_cor,"results/caffeine_correlations.rds")

