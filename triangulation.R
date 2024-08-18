# triangulate function and z-test

Z_test <- function(beta_poi,beta_cop,se_poi,se_cop){
  difference <- beta_poi - beta_cop
  se_difference <- sqrt(se_poi^2 + se_cop^2)
  z_score <- difference / se_difference
  z_score
  p_value <- 2 * pnorm(-abs(z_score))
  p_value
}


triangulate_results <- function(unstrat,parent,timing,comparisontiming){
## Creating MatPat again (because we need the original versions of exposure_class and exposure_time (not the versions used above))

mat <- unstrat[unstrat$person_exposed=="mother",]
pat <- unstrat[unstrat$person_exposed=="partner",]
matpat <- merge(mat,pat,by=c("exposure_class","exposure_subclass","exposure_time","exposure_type","exposure_dose","outcome_linker","model"),all=T,suffixes = c("mat","pat"))
matpat$samedir <- sign(matpat$estmat)==sign(matpat$estpat)
matpat$diff <- abs(matpat$estmat) - abs(matpat$estpat)
matpat$samedir_diff <- matpat$diff
matpat$samedir_diff[matpat$samedir==F]<-NA
matpat$samedir_pc_change <- (abs(matpat$samedir_diff)/abs(matpat$estmat))*100
matpat$pc_change <- (abs(matpat$diff)/abs(matpat$estmat))*100
matpat$Z_p <- apply(matpat[,c("estmat","estpat","semat","sepat")],1,function(x){
  Z_test(beta_poi=as.numeric(x[1]),beta_cop=as.numeric(x[2]),se_poi=as.numeric(x[3]),se_cop=as.numeric(x[4]))})

## MVR
top_hits_2a <- unstrat[unstrat$exposure_subclass!="genetic risk score"&unstrat$model=="model2a",c("person_exposed","exposure_class","exposure_subclass","exposure_time","exposure_type","exposure_dose","outcome_class","outcome_subclass1","outcome_subclass2","outcome_time","outcome_type","est","se","p","i2","hetp","cohorts","cohorts_n","total_n","fdr")] #model 2a for MVR,obs unstrata only

#top_hits_2a <- unstrat[unstrat$exposure_subclass!="genetic risk score"&unstrat$fdr<0.05&unstrat$model=="model2a",c("person_exposed","exposure_class","exposure_subclass","exposure_time","exposure_type","exposure_dose","outcome_class","outcome_subclass1","outcome_subclass2","outcome_time","outcome_type","est","se","p","i2","hetp","cohorts","cohorts_n","total_n","fdr")] #model 2a for MVR,obs unstrata only
top_hits_2a$combination <- apply(top_hits_2a[,c("outcome_class","outcome_subclass1","outcome_subclass2","outcome_time","outcome_type")],
                                 1,paste,collapse="_")
top_hits_2a_m <- top_hits_2a[top_hits_2a$person_exposed==parent&top_hits_2a$exposure_time%in%timing&top_hits_2a$exposure_type%in%c("continuous","binary"),]
tdat_s <- data.frame(outcome = top_hits_2a_m$combination[top_hits_2a_m$exposure_subclass=="active smoking"],
                     sign=sign(top_hits_2a_m$est[top_hits_2a_m$exposure_subclass=="active smoking"]),Psigmvr=top_hits_2a_m$p[top_hits_2a_m$exposure_subclass=="active smoking"]<0.05)
tdat_a <- data.frame(outcome = top_hits_2a_m$combination[top_hits_2a_m$exposure_subclass=="any drinking"],
                     sign=sign(top_hits_2a_m$est[top_hits_2a_m$exposure_subclass=="any drinking"]),Psigmvr=top_hits_2a_m$p[top_hits_2a_m$exposure_subclass=="any drinking"]<0.05)
tdat_c <- data.frame(outcome = top_hits_2a_m$combination[top_hits_2a_m$exposure_subclass=="any source"],
                     sign=sign(top_hits_2a_m$est[top_hits_2a_m$exposure_subclass=="any source"]),Psigmvr=top_hits_2a_m$p[top_hits_2a_m$exposure_subclass=="any source"]<0.05)

# calculating the median effect estimate (because we may have multiple effect estimates for each outcome, e.g. where the exposure was measured multiple times during pregnancy)
# then removing those where the median is 0 because this indicates no consensus in the direction
tdat_s <- tdat_s %>% group_by(outcome) %>% summarise(sign=median(sign),Psig=ceiling(sum(Psigmvr)/length(Psigmvr)))
#tdat_s <- tdat_s[which(tdat_s$sign%in%c(-1,1)),]
tdat_a <- tdat_a %>% group_by(outcome) %>% summarise(sign=median(sign),Psig=ceiling(sum(Psigmvr)/length(Psigmvr)))
#tdat_a <- tdat_a[which(tdat_a$sign%in%c(-1,1)),]
tdat_c <- tdat_c %>% group_by(outcome) %>% summarise(sign=median(sign),Psig=ceiling(sum(Psigmvr)/length(Psigmvr)))
#tdat_c <- tdat_c[which(tdat_c$sign%in%c(-1,1)),]

## GRS
top_hits_grs <- unstrat[unstrat$exposure_subclass=="genetic risk score"&unstrat$p<0.05&unstrat$model=="model2a",] #GRS any model (not specifying model because not clear which is best - adj for child outcome may be appropriate, but can introduce colider bias, adjusting for partner can potentially counteract, but not available for everyone, also analyses are underpowered, so it may be best to take the least adjusted model - DECIDED ON THAT IN THE END)
top_hits_grs$combination <- apply(top_hits_grs[,c("outcome_class","outcome_subclass1","outcome_subclass2","outcome_time","outcome_type")],
                                  1,paste,collapse="_")
top_hits_grs_m <- top_hits_grs[top_hits_grs$person_exposed==parent,]
tdat_s_grs <- data.frame(outcome = top_hits_grs_m$combination[top_hits_grs_m$exposure_class=="smoking"&top_hits_grs_m$exposure_time=="initiation"],
                         sign=sign(top_hits_grs_m$est[top_hits_grs_m$exposure_class=="smoking"&top_hits_grs_m$exposure_time=="initiation"]))
tdat_a_grs <- data.frame(outcome = top_hits_grs_m$combination[top_hits_grs_m$exposure_class=="alcohol consumption"],
                         sign=sign(top_hits_grs_m$est[top_hits_grs_m$exposure_class=="alcohol consumption"]))
tdat_c_grs <- data.frame(outcome = top_hits_grs_m$combination[top_hits_grs_m$exposure_class=="caffeine consumption"],
                         sign=sign(top_hits_grs_m$est[top_hits_grs_m$exposure_class=="caffeine consumption"]))

# calculating the median effect estimate (because we may have multiple effect estimates for each outcome, e.g. where the exposure was measured multiple times during pregnancy)
# then removing those where the median is 0 because this indicates no consensus in the direction
tdat_s_grs <- tdat_s_grs %>% group_by(outcome) %>% summarise(sign=median(sign))
tdat_s_grs <- tdat_s_grs[which(tdat_s_grs$sign%in%c(-1,1)),]
tdat_a_grs <- tdat_a_grs %>% group_by(outcome) %>% summarise(sign=median(sign))
tdat_a_grs <- tdat_a_grs[which(tdat_a_grs$sign%in%c(-1,1)),]
tdat_c_grs <- tdat_c_grs %>% group_by(outcome) %>% summarise(sign=median(sign))
tdat_c_grs <- tdat_c_grs[which(tdat_c_grs$sign%in%c(-1,1)),]

tdat_s <- merge(tdat_s,tdat_s_grs,by="outcome",all.x=T,all.y=T,suffixes=c("mvr","grs"))
tdat_a <- merge(tdat_a,tdat_a_grs,by="outcome",all.x=T,all.y=T,suffixes=c("mvr","grs"))
tdat_c <- merge(tdat_c,tdat_c_grs,by="outcome",all.x=T,all.y=T,suffixes=c("mvr","grs"))

## DOSE
heavy <- unstrat[which(unstrat$exposure_dose=="heavy"),]
light <- unstrat[which(unstrat$exposure_dose=="light"),]
heavylight <- merge(heavy,light,by=c("exposure_class","exposure_subclass","exposure_time","exposure_type", "person_exposed","outcome_linker","model"),all=T,suffixes = c("heavy","light"))
heavylight$diff <- abs(heavylight$estheavy) - abs(heavylight$estlight)
heavylight$pc_change <- (abs(heavylight$diff)/abs(heavylight$estheavy))*100

top_hits_dose_m <- heavylight[heavylight$exposure_subclass!="genetic risk score"&heavylight$pheavy<0.05&heavylight$diff>0 & heavylight$model=="model2a" &heavylight$person_exposed==parent,] #heavy>light, model2a
top_hits_dose_m$combination <- apply(top_hits_dose_m[,c("outcome_classheavy","outcome_subclass1heavy","outcome_subclass2heavy","outcome_timeheavy","outcome_typeheavy")],
                                     1,paste,collapse="_")
top_hits_dose_m <- top_hits_dose_m[top_hits_dose_m$exposure_time%in%c("first trimester","second trimester","third trimester"),]

tdat_s_dose <- data.frame(outcome = top_hits_dose_m$combination[top_hits_dose_m$exposure_subclass=="active smoking"],
                          signdose=sign(top_hits_dose_m$estheavy[top_hits_dose_m$exposure_subclass=="active smoking"]))
tdat_a_dose <- data.frame(outcome = top_hits_dose_m$combination[top_hits_dose_m$exposure_subclass=="any drinking"],
                          signdose=sign(top_hits_dose_m$estheavy[top_hits_dose_m$exposure_subclass=="any drinking"]))
tdat_c_dose <- data.frame(outcome = top_hits_dose_m$combination[top_hits_dose_m$exposure_subclass=="any source"],
                          signdose=sign(top_hits_dose_m$estheavy[top_hits_dose_m$exposure_subclass=="any source"]))

# calculating the median effect estimate (because we may have multiple effect estimates for each outcome, e.g. where the exposure was measured multiple times during pregnancy)
# then removing those where the median is 0 because this indicates no consensus in the direction
tdat_s_dose <- tdat_s_dose %>% group_by(outcome) %>% summarise(signdose=median(signdose))
tdat_s_dose <- tdat_s_dose[which(tdat_s_dose$signdose%in%c(-1,1)),]
tdat_a_dose <- tdat_a_dose %>% group_by(outcome) %>% summarise(signdose=median(signdose))
tdat_a_dose <- tdat_a_dose[which(tdat_a_dose$signdose%in%c(-1,1)),]
tdat_c_dose <- tdat_c_dose %>% group_by(outcome) %>% summarise(signdose=median(signdose))
tdat_c_dose <- tdat_c_dose[which(tdat_c_dose$signdose%in%c(-1,1)),]

tdat_s <- merge(tdat_s,tdat_s_dose,by="outcome",all.x=T,all.y=T)
tdat_a <- merge(tdat_a,tdat_a_dose,by="outcome",all.x=T,all.y=T)
tdat_c <- merge(tdat_c,tdat_c_dose,by="outcome",all.x=T,all.y=T)

## TIMING (comparing the time of interest to postnatal or during pregnancy)
ModelTime <- "model2a"
#if(any(timing%in%"preconception")){ModelTime <- "model2a"} #take the model adjusted for previous timepoints (3a), but if timing==preconception, there is no previous timepoint, so take model2a for that one
timeofinterest <- unstrat[which(unstrat$exposure_time%in%timing&unstrat$model==ModelTime),]

ComparisonTime <-c("ever in pregnancy","first trimester","second trimester","third trimester")
if(any(comparisontiming%in%"postnatal")){ComparisonTime <- "first two postnatal years"} #comparison time is during pregnancy or postnatal
ModelComparisonTime <-"model3a"
comparisontime <- unstrat[which(unstrat$exposure_time==ComparisonTime&unstrat$model==ModelComparisonTime),]

timeofinterestcomparisontime <- merge(timeofinterest,comparisontime,by=c("exposure_class","exposure_subclass","exposure_dose","exposure_type", "person_exposed","outcome_linker"),all=T,suffixes = c("timeofinterest","comparisontime"))
timeofinterestcomparisontime$diff <- abs(timeofinterestcomparisontime$esttimeofinterest) - abs(timeofinterestcomparisontime$estcomparisontime)
timeofinterestcomparisontime$pc_change <- (abs(timeofinterestcomparisontime$diff)/abs(timeofinterestcomparisontime$esttimeofinterest))*100
top_hits_time_m <- timeofinterestcomparisontime[which(timeofinterestcomparisontime$exposure_subclass!="genetic risk score"& timeofinterestcomparisontime$ptimeofinterest<0.05&timeofinterestcomparisontime$diff>0 &timeofinterestcomparisontime$person_exposed==parent),] #timeofinterest>comparisontime, model3b
top_hits_time_m$combination <- apply(top_hits_time_m[,c("outcome_classtimeofinterest","outcome_subclass1timeofinterest","outcome_subclass2timeofinterest","outcome_timetimeofinterest","outcome_typetimeofinterest")],
                                     1,paste,collapse="_")

tdat_s_time <- data.frame(outcome = top_hits_time_m$combination[top_hits_time_m$exposure_subclass=="active smoking"],
                          signtime=sign(top_hits_time_m$esttimeofinterest[top_hits_time_m$exposure_subclass=="active smoking"]))
tdat_a_time <- data.frame(outcome = top_hits_time_m$combination[top_hits_time_m$exposure_subclass=="any drinking"],
                          signtime=sign(top_hits_time_m$esttimeofinterest[top_hits_time_m$exposure_subclass=="any drinking"]))
tdat_c_time <- data.frame(outcome = top_hits_time_m$combination[top_hits_time_m$exposure_subclass=="any source"],
                          signtime=sign(top_hits_time_m$esttimeofinterest[top_hits_time_m$exposure_subclass=="any source"]))

# calculating the median effect estimate (because we may have multiple effect estimates for each outcome, e.g. where the exposure was measured multiple times during pregnancy)
# then removing those where the median is 0 because this indicates no consensus in the direction
tdat_s_time <- tdat_s_time %>% group_by(outcome) %>% summarise(signtime=median(signtime))
#tdat_s_time <- tdat_s_time[which(tdat_s_time$signtime%in%c(-1,1)),]
tdat_a_time <- tdat_a_time %>% group_by(outcome) %>% summarise(signtime=median(signtime))
#tdat_a_time <- tdat_a_time[which(tdat_a_time$signtime%in%c(-1,1)),]
tdat_c_time <- tdat_c_time %>% group_by(outcome) %>% summarise(signtime=median(signtime))
#tdat_c_time <- tdat_c_time[which(tdat_c_time$signtime%in%c(-1,1)),]

tdat_s <- merge(tdat_s,tdat_s_time,by="outcome",all.x=T,all.y=T)
tdat_a <- merge(tdat_a,tdat_a_time,by="outcome",all.x=T,all.y=T)
tdat_c <- merge(tdat_c,tdat_c_time,by="outcome",all.x=T,all.y=T)

# Neg Con
if(parent=="mother"){
matpat$poi_p <-matpat$pmat
matpat$poi_diff <-matpat$samedir_diff>0
matpat$poi_est <- matpat$estmat
}else{
  matpat$poi_p <-matpat$ppat
  matpat$poi_diff <-matpat$samedir_diff<0
  matpat$poi_est <- matpat$estpat
}

top_hits_negcon_m <- matpat[matpat$exposure_subclass!="genetic risk score"&matpat$poi_p<0.05 & matpat$samedir==T & matpat$poi_diff==T& matpat$model=="model2b" ,] #mutually adjusted model, mat>pat and in same dir, and matpat p<0.05, and P for diff between estimates Z_p<0.05.

top_hits_negcon_m$combination <- apply(top_hits_negcon_m[,c("outcome_classmat","outcome_subclass1mat","outcome_subclass2mat","outcome_timemat","outcome_typemat")],
                                       1,paste,collapse="_")
top_hits_negcon_m <- top_hits_negcon_m[top_hits_negcon_m$exposure_time%in%timing,]

tdat_s_negcon <- data.frame(outcome = top_hits_negcon_m$combination[top_hits_negcon_m$exposure_subclass=="active smoking"],
                            signnegcon=sign(top_hits_negcon_m$poi_est[top_hits_negcon_m$exposure_subclass=="active smoking"]))
tdat_a_negcon <- data.frame(outcome = top_hits_negcon_m$combination[top_hits_negcon_m$exposure_subclass=="any drinking"],
                            signnegcon=sign(top_hits_negcon_m$poi_est[top_hits_negcon_m$exposure_subclass=="any drinking"]))
tdat_c_negcon <- data.frame(outcome = top_hits_negcon_m$combination[top_hits_negcon_m$exposure_subclass=="any source"],
                            signnegcon=sign(top_hits_negcon_m$poi_est[top_hits_negcon_m$exposure_subclass=="any source"]))

# calculating the median effect estimate (because we may have multiple effect estimates for each outcome, e.g. where the exposure was measured multiple times during pregnancy)
# then removing those where the median is 0 because this indicates no consensus in the direction
tdat_s_negcon <- tdat_s_negcon %>% group_by(outcome) %>% summarise(signnegcon=median(signnegcon))
#tdat_s_negcon <- tdat_s_negcon[which(tdat_s_negcon$signnegcon%in%c(-1,1)),]
tdat_a_negcon <- tdat_a_negcon %>% group_by(outcome) %>% summarise(signnegcon=median(signnegcon))
#tdat_a_negcon <- tdat_a_negcon[which(tdat_a_negcon$signnegcon%in%c(-1,1)),]
tdat_c_negcon <- tdat_c_negcon %>% group_by(outcome) %>% summarise(signnegcon=median(signnegcon))
#tdat_c_negcon <- tdat_c_negcon[which(tdat_c_negcon$signnegcon%in%c(-1,1)),]

tdat_s <- merge(tdat_s,tdat_s_negcon,by="outcome",all.x=T,all.y=T)
tdat_a <- merge(tdat_a,tdat_a_negcon,by="outcome",all.x=T,all.y=T)
tdat_c <- merge(tdat_c,tdat_c_negcon,by="outcome",all.x=T,all.y=T)

#recode MVR Psig column so that it doesn't get mixed up with the next bit where we rename sign
tdat_s$Psig[tdat_s$Psig==1]<-"yes"
tdat_a$Psig[tdat_a$Psig==1]<-"yes"
tdat_c$Psig[tdat_c$Psig==1]<-"yes"
tdat_s$Psig[tdat_s$Psig==0]<-"no"
tdat_a$Psig[tdat_a$Psig==0]<-"no"
tdat_c$Psig[tdat_c$Psig==0]<-"no"

#recode NA to 0
tdat_s[is.na(tdat_s)]<-0
tdat_a[is.na(tdat_a)]<-0
tdat_c[is.na(tdat_c)]<-0

#set signmvr2 to 0 if MVR is not significant at P<0.05
tdat_s$signmvr2 <-tdat_s$signmvr
tdat_s$signmvr2[tdat_s$Psig=="no"]<-0
tdat_a$signmvr2 <-tdat_a$signmvr
tdat_a$signmvr2[tdat_a$Psig=="no"]<-0
tdat_c$signmvr2 <-tdat_c$signmvr
tdat_c$signmvr2[tdat_c$Psig=="no"]<-0

## summarise lines of evidence

tdat_s$totaln <- apply(tdat_s[,c("signmvr2","signgrs","signdose","signtime","signnegcon")],1,function(x){abs(sum(x[x==median(x[x!=0])]))})
tdat_s$totaln[is.na(tdat_s$totaln)]<-0
tdat_a$totaln <- apply(tdat_a[,c("signmvr2","signgrs","signdose","signtime","signnegcon")],1,function(x){abs(sum(x[x==median(x[x!=0])]))})
tdat_a$totaln[is.na(tdat_a$totaln)]<-0
tdat_c$totaln <- apply(tdat_c[,c("signmvr2","signgrs","signdose","signtime","signnegcon")],1,function(x){abs(sum(x[x==median(x[x!=0])]))})
tdat_c$totaln[is.na(tdat_c$totaln)]<-0

tdat_s$total <-NA
tdat_a$total <-NA
tdat_c$total <-NA

if(nrow(tdat_s)>0){
tdat_s$total <- "0 lines\nof evidence"
tdat_s$total[tdat_s$totaln==1]<-"1 line\nof evidence"
tdat_s$total[tdat_s$totaln==2]<-"2 lines\nof evidence"
tdat_s$total[tdat_s$totaln==3]<-"3 lines\nof evidence"
tdat_s$total[tdat_s$totaln==4]<-"4 lines\nof evidence"
tdat_s$total[tdat_s$totaln==5]<-"5 lines\nof evidence"
tdat_s$totaln<-NULL
}

if(nrow(tdat_a)>0){
tdat_a$total <- "0 lines\nof evidence"
tdat_a$total[tdat_a$totaln==1]<-"1 line\nof evidence"
tdat_a$total[tdat_a$totaln==2]<-"2 lines\nof evidence"
tdat_a$total[tdat_a$totaln==3]<-"3 lines\nof evidence"
tdat_a$total[tdat_a$totaln==4]<-"4 lines\nof evidence"
tdat_a$total[tdat_a$totaln==5]<-"5 lines\nof evidence"
tdat_a$totaln<-NULL
}

if(nrow(tdat_c)>0){
tdat_c$total <- "0 lines\nof evidence"
tdat_c$total[tdat_c$totaln==1]<-"1 line\nof evidence"
tdat_c$total[tdat_c$totaln==2]<-"2 lines\nof evidence"
tdat_c$total[tdat_c$totaln==3]<-"3 lines\nof evidence"
tdat_c$total[tdat_c$totaln==4]<-"4 lines\nof evidence"
tdat_c$total[tdat_c$totaln==5]<-"5 lines\nof evidence"
tdat_c$totaln<-NULL
}

## rename sign 

tdat_s[tdat_s==(-1)]<-"negative\nassociation"
tdat_s[tdat_s==(1)]<-"positive\nassociation"
tdat_s[tdat_s==(0)]<-NA
tdat_c[tdat_c==(-1)]<-"negative\nassociation"
tdat_c[tdat_c==(1)]<-"positive\nassociation"
tdat_c[tdat_c==(0)]<-NA
tdat_a[tdat_a==(-1)]<-"negative\nassociation"
tdat_a[tdat_a==(1)]<-"positive\nassociation"
tdat_a[tdat_a==(0)]<-NA

# melt
tdat_s_melt <- reshape2::melt(tdat_s[,-which(colnames(tdat_s) %in% c("signmvr","Psig"))],na.rm = F,id.vars="outcome")
tdat_s_melt$exposure<-"smoking"
tdat_s_melt$parent<-parent
tdat_a_melt <- reshape2::melt(tdat_a[,-which(colnames(tdat_a) %in% c("signmvr","Psig"))],na.rm = F,id.vars="outcome")
tdat_a_melt$exposure<-"alcohol consumption"
tdat_a_melt$parent<-parent
tdat_c_melt <- reshape2::melt(tdat_c[,-which(colnames(tdat_c) %in% c("signmvr","Psig"))],na.rm = F,id.vars="outcome")
tdat_c_melt$exposure<-"caffeine consumption"
tdat_c_melt$parent<-parent

tdat <- bind_rows(mget(intersect(ls(), c("tdat_s_melt","tdat_a_melt","tdat_c_melt"))))

tdat$outcome_class <- unlist(lapply(lapply(lapply(tdat$outcome,stringr::str_split,pattern = "_"),unlist),"[",1))
tdat$outcome_subclass2 <- unlist(lapply(lapply(lapply(tdat$outcome,stringr::str_split,pattern = "_"),unlist),"[",3))
tdat$outcome_subclass <- paste(unlist(lapply(lapply(lapply(tdat$outcome,stringr::str_split,pattern = "_"),unlist),"[",3)),
                                       unlist(lapply(lapply(lapply(tdat$outcome,stringr::str_split,pattern = "_"),unlist),"[",4)),
                                       sep=" at ") 

tdat$value <- factor(tdat$value,ordered=T,levels=c("5 lines\nof evidence","4 lines\nof evidence","3 lines\nof evidence","2 lines\nof evidence","1 line\nof evidence","0 lines\nof evidence","negative\nassociation","positive\nassociation"))
tdat$exposure <- factor(tdat$exposure,ordered=T,levels=c("smoking","alcohol consumption","caffeine consumption"))
tdat<-tdat[order(tdat$outcome_class,tdat$outcome_subclass),]
tdat$outcome_subclass <- factor(tdat$outcome_subclass,ordered=T,levels=unique(tdat$outcome_subclass))

tdat$variable2<-NA
tdat$variable2[tdat$variable=="signmvr2"]<-"M"
tdat$variable2[tdat$variable=="signgrs"]<-"G"
tdat$variable2[tdat$variable=="signnegcon"]<-"N"
tdat$variable2[tdat$variable=="signdose"]<-"D"
tdat$variable2[tdat$variable=="signtime"]<-"P"
tdat$variable2[tdat$variable=="total"]<-"T"
tdat$variable2 <- factor(tdat$variable2,ordered=T,levels=c("M","N","G","D","P","T"))

tdat
}

combine_triang_results<-function(x,y){
  both <- rbind(x,y)
  both$variable2 <- factor(both$variable2,ordered=T,levels=c("M","D","N","P","G","T"))
  both$T <- "No"
  both$T[both$variable2=="T"]<-"Yes"
  both
}
