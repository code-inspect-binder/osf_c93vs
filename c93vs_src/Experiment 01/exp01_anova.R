#------------------------------------------------------------------------------#
#This file contains R-code for data analyses of Experiment 1 reported in:      #
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

setwd("C:/Users/Stefan/Downloads/osfstorage-archive/Experiment 01") #set to location of the data files

#------data analyses------#
#import data
#variant 1 (choose only one)
#dat = read.table(file = "exp01_data.txt", header = TRUE)
#dat$subj = as.factor(dat$subj)
#dat$soa = as.factor(dat$soa)

#variant 2 (choose only one)
load("exp01_prep.RData")

#variant 3 (choose only one)
#dat = read.table(file = "https://osf.io/hkpv8/download", header = TRUE)
#dat$subj = as.factor(dat$subj)
#dat$soa = as.factor(dat$soa)

#data structure
str(dat)
#- targ_ID = target sound  ID
#- cond = distractor condition (congruent; semantic; unrelated)
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

#----------------------------ANOVA: cond x soa------------------------------------------------
#-------------aggregate---------
agg.subj = ddply(dat,.(subj, soa, cond), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.item = ddply(dat,.(targ_ID, soa, cond), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.subj$PCT = agg.subj$ERR / length(levels(dat$targ_ID)) * 100
agg.item$PCT = agg.item$ERR / length(levels(dat$subj)) * 100

#-------------------------------------------------------------latency analyses----------------
#descriptive
with(agg.subj,describeBy(RT,group=list(soa, cond),mat=T))

#------------------------participant analysis
ezANOVA(data=agg.subj, dv=.("RT"), wid=.("subj"), within=.("soa", "cond"),detailed=T,type=3)
#Greenhouse-Geisser correction - df
aov_ez(data=agg.subj, dv=.("RT"), id=.("subj"), within=.("soa", "cond"))

#-------------------------------item analysis
ezANOVA(data=agg.item, dv=.("RT"), wid=.("targ_ID"), within=.("soa", "cond"),detailed=T,type=3)
#Greenhouse-Geisser correction - df
aov_ez(data=agg.item, dv=.("RT"), id=.("targ_ID"), within=.("soa", "cond"))

#--------------------------------------------------------------error analyses-----------------
#descriptive
with(agg.subj,describeBy(PCT,group=list(soa, cond),mat=T))

#------------------------participant analysis
ezANOVA(data=agg.subj, dv=.("PCT"), wid=.("subj"), within=.("soa", "cond"),detailed=T,type=3)

#-------------------------------item analysis
ezANOVA(data=agg.item, dv=.("PCT"), wid=.("targ_ID"), within=.("soa", "cond"),detailed=T,type=3)

#----------------------------t-tests: cond------------------------------------------------
#-------------aggregate---------
agg.subj_C = ddply(dat,.(subj, cond), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.item_C = ddply(dat,.(targ_ID, cond), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.subj_C$PCT = agg.subj_C$ERR / (length(levels(dat$targ_ID))*2) * 100
agg.item_C$PCT = agg.item_C$ERR / (length(levels(dat$subj))*2) * 100

#reshape data - participants
agg.subj.wide_C = longToWide (agg.subj_C,
                              RT + ERR + PCT ~ cond, sep = ".")

#reshape data - items
agg.item.wide_C = longToWide (agg.item_C,
                              RT + ERR + PCT ~ cond, sep = ".")

#-------------------------------------------------------------latency analyses----------------
#descriptive
with(agg.subj_C,describeBy(RT,group=list(cond),mat=T))

#-------------------------------congruency facilitation effect

#------------------------participant analysis
res = t.test(agg.subj.wide_C$RT.congruent, agg.subj.wide_C$RT.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_C$RT.congruent, agg.subj.wide_C$RT.unrelated, method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_C$RT.congruent, agg.item.wide_C$RT.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_C$RT.congruent, agg.item.wide_C$RT.unrelated, method = "paired")

#---------------------------------semantic interference effect

#------------------------participant analysis
res = t.test(agg.subj.wide_C$RT.semantic, agg.subj.wide_C$RT.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_C$RT.semantic, agg.subj.wide_C$RT.unrelated, method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_C$RT.semantic, agg.item.wide_C$RT.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_C$RT.semantic, agg.item.wide_C$RT.unrelated, method = "paired")

#--------------------------------------------------------------error analyses-----------------
#descriptive
with(agg.subj_C,describeBy(PCT,group=list(cond),mat=T))

#-------------------------------congruency facilitation effect

#------------------------participant analysis
res = t.test(agg.subj.wide_C$PCT.congruent, agg.subj.wide_C$PCT.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_C$PCT.congruent, agg.subj.wide_C$PCT.unrelated, method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_C$PCT.congruent, agg.item.wide_C$PCT.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_C$PCT.congruent, agg.item.wide_C$PCT.unrelated, method = "paired")

#---------------------------------semantic interference effect

#------------------------participant analysis
res = t.test(agg.subj.wide_C$PCT.semantic, agg.subj.wide_C$PCT.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_C$PCT.semantic, agg.subj.wide_C$PCT.unrelated, method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_C$PCT.semantic, agg.item.wide_C$PCT.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_C$PCT.semantic, agg.item.wide_C$PCT.unrelated, method = "paired")

###################################
#save workspace
save.image(file="exp01_anova.RData")