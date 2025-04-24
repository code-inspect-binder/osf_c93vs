#------------------------------------------------------------------------------#
#This file contains R-code for data analyses of Experiment 7 reported in:      #
# Wöhner, S. (2018).                                                           #
#    #Natürliche Geräusche und Bilder in Benennungsaufgaben -                  #
#     Semantische Kontexteffekte innerhalb und zwischen  Stimulusmodalitäten#  #
#                                                                              #
#(c) Stefan Wöhner (stefan.woehner@uni-leipzig.de)                             #
#------------------------------------------------------------------------------

#------Setup-------#
rm(list = ls())

#packages (install before)
library(ez)
library(lsr)
library(afex)
library(psych)
library(plyr)
options(scipen=999)

setwd("C:/Users/Stefan/Downloads/osfstorage-archive/Experiment 07") #set to location of the data files

#------data analyses------#
#import data
#variant 1 (choose only one)
#dat = read.table(file = "exp07_data.txt", header = TRUE)
#dat$subj = as.factor(dat$subj)
#dat$soa = as.factor(dat$soa)

#variant 2 (choose only one)
load("exp07_prep.RData")

#variant 3 (choose only one)
#dat = read.table(file = "https://osf.io/h67fw/download", header = TRUE)
#dat$subj = as.factor(dat$subj)
#dat$soa = as.factor(dat$soa)

#data structure
str(dat)
#- targ_ID = target picture  ID
#- rel = type of relation (congruent; semantic)
#- rtd = relatedness (related; unrelated)
#- soa = stimulus-onset-asynchrony (-200 ms; 0 ms)
#- subj = participant ID
#- trial = trial number
#- RT = naming latency
#- ERR.code = error code
#- dist_ID = distractor picture ID
#- blk = experimental block
#- ERR.old = including participant and technical errors
#- ERR = excluding technical (non-participant) errors
#- RT.old = naming latency before error/outlier exclusion
#- out = outlier
#- exp = experiment number

#------------errors/outliers-----------
#N discarded RTs overall
table(is.na(dat$RT))
table(is.na(dat$RT)) / length(dat$RT) * 100

#N discarded RTs <= 300 ms and RTs >= 3,000 ms
table(is.na(dat$RT) & dat$out!=1 & dat$ERR.old==0)
table(is.na(dat$RT) & dat$out!=1 & dat$ERR.old==0) / length(dat$ERR.old) * 100

#N naming errors (including technical errors)
table(is.na(dat$RT) & dat$ERR.old)
table(is.na(dat$RT) & dat$ERR.old) / length(dat$ERR.old) * 100

#N naming errors (excluding technical errors)
table(is.na(dat$RT) & dat$ERR)
table(is.na(dat$RT) & dat$ERR) / length(dat$ERR) * 100

#N outliers
table(is.na(dat$RT) & dat$out)
table(is.na(dat$RT) & dat$out) / length(dat$out) * 100

#----------------------------ANOVA: rtd x soa, separated by rel----------------------------------------------

#separate data set by type of relation (congruent; semantic)
dat.con = dat[dat$rel=="congruent",]
dat.sem = dat[dat$rel=="semantic",]

#-------------aggregate---------
agg.subj_con = ddply(dat.con,.(subj, soa, rtd), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.item_con = ddply(dat.con,.(targ_ID, soa, rtd), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.subj_con$PCT = agg.subj_con$ERR / (length(levels(dat$targ_ID))/2) * 100
agg.item_con$PCT = agg.item_con$ERR / (length(levels(dat$subj))/2) * 100

agg.subj_sem = ddply(dat.sem,.(subj, soa, rtd), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.item_sem = ddply(dat.sem,.(targ_ID, soa, rtd), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.subj_sem$PCT = agg.subj_sem$ERR / (length(levels(dat$targ_ID))/2) * 100
agg.item_sem$PCT = agg.item_sem$ERR / (length(levels(dat$subj))/2) * 100

agg.subj_con_C = ddply(dat.con,.(subj, rtd), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.item_con_C = ddply(dat.con,.(targ_ID, rtd), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.subj_con_C$PCT = agg.subj_con_C$ERR / length(levels(dat$targ_ID)) * 100
agg.item_con_C$PCT = agg.item_con_C$ERR / length(levels(dat$subj)) * 100

agg.subj_sem_C = ddply(dat.sem,.(subj, rtd), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.item_sem_C = ddply(dat.sem,.(targ_ID, rtd), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.subj_sem_C$PCT = agg.subj_sem_C$ERR / length(levels(dat$targ_ID)) * 100
agg.item_sem_C$PCT = agg.item_sem_C$ERR / length(levels(dat$subj)) * 100

#-----------------------------------------------------------------type of relation = congruent

#-------------------------------------------------------------latency analyses----------------
#descriptive
with(agg.subj_con,describeBy(RT,group=list(soa, rtd),mat=T))
with(agg.subj_con_C,describeBy(RT,group=list(rtd),mat=T))

#------------------------participant analysis
ezANOVA(data=agg.subj_con, dv=.("RT"), wid=.("subj"), within=.("soa", "rtd"),detailed=T,type=3)

#-------------------------------item analysis
ezANOVA(data=agg.item_con, dv=.("RT"), wid=.("targ_ID"), within=.("soa", "rtd"),detailed=T,type=3)

#--------------------------------------------------------------error analyses-----------------

#descriptive
with(agg.subj_con,describeBy(PCT,group=list(soa, rtd),mat=T))
with(agg.subj_con_C,describeBy(PCT,group=list(rtd),mat=T))

#------------------------participant analysis
ezANOVA(data=agg.subj_con, dv=.("PCT"), wid=.("subj"), within=.("soa", "rtd"),detailed=T,type=3)

#-------------------------------item analysis
ezANOVA(data=agg.item_con, dv=.("PCT"), wid=.("targ_ID"), within=.("soa", "rtd"),detailed=T,type=3)

#----------------------------t-tests: rtd x soa-----------------------------------------------
#reshape data - participants
agg.subj.wide_con = longToWide (agg.subj_con,
                                RT + ERR + PCT ~ soa | rtd, sep = ".")

#reshape data - items
agg.item.wide_con = longToWide (agg.item_con,
                                RT + ERR + PCT ~ soa | rtd, sep = ".")

#--------------------------------------------------------------error analyses-----------------

#soa = -200 ms

#------------------------participant analysis
res = t.test(agg.subj.wide_con$'PCT.-200.related', agg.subj.wide_con$'PCT.-200.unrelated', paired=TRUE)
#SEM = res$estimate[[1]]/res$statistic[[1]]#does not work for mean differences equal to zero; but see formula below
SEM=sd(agg.subj.wide_con$'PCT.-200.related'-agg.subj.wide_con$'PCT.-200.unrelated')/sqrt(length(levels(dat$subj)))
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_con$'PCT.-200.related', agg.subj.wide_con$'PCT.-200.unrelated', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_con$'PCT.-200.related', agg.item.wide_con$'PCT.-200.unrelated', paired=TRUE)
#SEM = res$estimate[[1]]/res$statistic[[1]]#does not work for mean differences equal to zero; but see formula below
SEM=sd(agg.item.wide_con$'PCT.-200.related'-agg.item.wide_con$'PCT.-200.unrelated')/sqrt(length(levels(dat$targ_ID)))
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_con$'PCT.-200.related', agg.item.wide_con$'PCT.-200.unrelated', method = "paired")

#soa = 0 ms

#------------------------participant analysis
res = t.test(agg.subj.wide_con$'PCT.0.related', agg.subj.wide_con$'PCT.0.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_con$'PCT.0.related', agg.subj.wide_con$'PCT.0.unrelated', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_con$'PCT.0.related', agg.item.wide_con$'PCT.0.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_con$'PCT.0.related', agg.item.wide_con$'PCT.0.unrelated', method = "paired")

#------------------------------------------------------------------type of relation = semantic

#-------------------------------------------------------------latency analyses----------------
#descriptive
with(agg.subj_sem,describeBy(RT,group=list(soa, rtd),mat=T))
with(agg.subj_sem_C,describeBy(RT,group=list(rtd),mat=T))

#------------------------participant analysis
ezANOVA(data=agg.subj_sem, dv=.("RT"), wid=.("subj"), within=.("soa", "rtd"),detailed=T,type=3)

#-------------------------------item analysis
ezANOVA(data=agg.item_sem, dv=.("RT"), wid=.("targ_ID"), within=.("soa", "rtd"),detailed=T,type=3)

#--------------------------------------------------------------error analyses-----------------
#descriptive
with(agg.subj_sem,describeBy(PCT,group=list(soa, rtd),mat=T))
with(agg.subj_sem_C,describeBy(PCT,group=list(rtd),mat=T))

#------------------------participant analysis
ezANOVA(data=agg.subj_sem, dv=.("PCT"), wid=.("subj"), within=.("soa", "rtd"),detailed=T,type=3)

#-------------------------------item analysis
ezANOVA(data=agg.item_sem, dv=.("PCT"), wid=.("targ_ID"), within=.("soa", "rtd"),detailed=T,type=3)

###################################
#save workspace
save.image(file="exp07_anova.RData")