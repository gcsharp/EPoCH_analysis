
dat <- readRDS("~/OneDrive - University of Exeter/Projects/EPoCH/EPoCH results app/data/rds/all_results_reduced.rds")
require(ggplot2)
require(ggh4x)

#EXPOSURES 
dat$y <- dat$exposure_time
dat$x <- dat$exposure_subclass
dat$y[dat$exposure_subclass=="genetic risk score"]<-"ever in life"
dat$x[dat$exposure_subclass=="genetic risk score"]<-paste0("GRS: ",dat$exposure_time[dat$exposure_subclass=="genetic risk score"])
require(tidyverse)
tb <- dat %>%
  group_by(x,y,person_exposed,exposure_class) %>%
  reframe(ALSPAC=all(is.na(est_ALSPAC))==F,BIB=all(is.na(est_BIB))==F,MCS=all(is.na(est_MCS))==F,MOBA=all(is.na(est_MOBA))==F)
tb <- tb %>% gather(key="cohort", value="present", 5:8)
tb$present2 <-NA
tb$present2[tb$present==T]<-"available"
tb$present2[tb$present==F]<-"not available"
tb$exposure_class[tb$exposure_class=="low socioeconomic position"]<-"SEP"
tb$exposure_class[tb$exposure_class=="alcohol consumption"]<-"alcohol"
tb$exposure_class[tb$exposure_class=="caffeine consumption"]<-"caffeine"
tb$exposure_class<-factor(tb$exposure_class,ordered=T,levels=c("smoking","alcohol","caffeine","SEP"))
tb$y<-factor(tb$y,ordered=T,levels=c("at study recruitment","early onset","ever in life","preconception","ever in pregnancy","first trimester","second trimester","third trimester","first two postnatal years"))

tb->exposure_tb

ggplot(exposure_tb,aes(x=y,y=x,fill=present2))+
  geom_tile(colour="white")+
  scale_fill_manual(values=c("aquamarine3","grey50"))+
  facet_nested(person_exposed+exposure_class~cohort,scales = "free_y",space = "free_y")+
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.title=element_blank(),legend.title=element_blank(),legend.position = "bottom",
        strip.text.y=element_text(angle=0,hjust=0))

#OUTCOMES 

dat$y <- dat$outcome_subclass2
dat$x <- dat$outcome_time
tb <- dat %>%
  group_by(x,y,outcome_class) %>%
  reframe(ALSPAC=all(is.na(est_ALSPAC))==F,BIB=all(is.na(est_BIB))==F,MCS=all(is.na(est_MCS))==F,MOBA=all(is.na(est_MOBA))==F)
tb <- tb %>% gather(key="cohort", value="present", 4:7)
tb$present2 <-NA
tb$present2[tb$present==T]<-"available"
tb$present2[tb$present==F]<-"not available"
tb$x<-factor(tb$x,ordered=T,levels=c("delivery","first year","age 1 to 2","age 3 to 4","age 5 to 7","age 8 to 11","any time in childhood"))
tb->outcome_tb

pdf("~/OneDrive - University of Exeter/Projects/EPoCH/data_availability_outcomes.pdf",width = 10,height = 15)
ggplot(outcome_tb,aes(x=x,y=y,fill=present2))+
  geom_tile(colour="white")+
  scale_fill_manual(values=c("aquamarine3","grey50"))+
  facet_nested(outcome_class~cohort,scales = "free_y",space = "free_y")+
  theme(axis.text.x=element_text(angle=90,hjust=1),axis.title=element_blank(),
        strip.text.y=element_text(angle=0,hjust=0),legend.title=element_blank(),legend.position = "bottom")
dev.off()
