#------------------------------------------------------------------------------#
#This file contains R-code for data analyses of Experiment 3 reported in:      #
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

setwd("C:/Users/Stefan/Downloads/osfstorage-archive/Experiment 03") #set to location of the data files

#------data analyses------#
#import data
#variant 1 (choose only one)
#dat = read.table(file = "exp03_data.txt", header = TRUE)
#dat$subj = as.factor(dat$subj)
#dat$task.soa = as.factor(dat$task.soa)

#variant 2 (choose only one)
load("exp03_prep.RData")

#variant 3 (choose only one)
#dat = read.table(file = "https://osf.io/xpb86/download", header = TRUE)
#dat$subj = as.factor(dat$subj)
#dat$task.soa = as.factor(dat$task.soa)

#data structure
str(dat)
#- targ_ID = target sound  ID
#- cond = distractor condition (congruent; semantic; unrelated)
#- task.soa = task stimulus-onset-asynchrony (0 ms; 1,500 ms)
#- subj = participant ID
#- trial = trial number
#- RT.task1 = response latency (task 1)
#- ERR.code.task1 = error code (task 1)
#- RT.task2 = naming latency (task 2)
#- ERR.code.task2 = error code (task 2)
#- task1.let = letter (B; F; G; K; L; P; Q; Z)
#- task1.let_ver = letter version (normal; mirrored)
#- dist_ID = distractor picture ID
#- blk = experimental block
#- ERR.task1 = errors (task 1)
#- ERR.task2.old = including participant and technical errors (task 2)
#- ERR.task2 = excluding technical (non-participant) errors (task 2)
#- RT.task1.old = response latency before error/outlier exclusion (task 1)
#- RT.task2.old = naming latency before error/outlier exclusion (task 2)
#- out = outlier
#- exp = experiment number

#------------errors/outliers-----------
#N discarded RTs overall
#task 1
table(is.na(dat$RT.task1))
table(is.na(dat$RT.task1)) / length(dat$RT.task1) * 100
#task 2
table(is.na(dat$RT.task2))
table(is.na(dat$RT.task2)) / length(dat$RT.task2) * 100

#N discarded RTs <= 100 ms and RTs >= 3,000/4,500 ms
#task 1
dat$RT.task1_fs=0
dat$RT.task1_fs[dat$task.soa==0 & dat$RT.task1.old>=3000 & dat$ERR.task1==0 | 
                dat$task.soa==1500 & dat$RT.task1.old>=4500 & dat$ERR.task1==0 |
                dat$RT.task1.old<=100 & dat$ERR.task1==0]=1
table(dat$RT.task1_fs)

#N errors
#task 1
table(is.na(dat$RT.task1) & dat$ERR.task1)
table(is.na(dat$RT.task1) & dat$ERR.task1) / length(dat$ERR.task1) * 100

#N discarded RTs <= 300 ms and RTs >= 3,000 ms
#task 2
dat$RT.task2_fs=0
dat$RT.task2_fs[dat$RT.task2.old<=300 & dat$ERR.task2.old==0 | 
                  dat$RT.task2.old>=3000 & dat$ERR.task2.old==0]=1
table(dat$RT.task2_fs)

#N naming errors (including technical errors)
#task 2
table(is.na(dat$RT.task2) & dat$ERR.task2.old)
table(is.na(dat$RT.task2) & dat$ERR.task2.old) / length(dat$ERR.task2.old) * 100

#N naming errors (excluding technical errors)
#task 2
table(is.na(dat$RT.task2) & dat$ERR.task2)
table(is.na(dat$RT.task2) & dat$ERR.task2) / length(dat$ERR.task2) * 100

#N outliers
table(is.na(dat$RT.task2) & dat$out)
table(is.na(dat$RT.task2) & dat$out) / length(dat$out) * 100

#-----------------------------------------------TASK 1: letter decision-------------------------------#
#----------------------------ANOVA: cond x task.soa------------------------------------------------
#-------------aggregate---------
agg.subj.task1 = ddply(dat,.(subj, task.soa, cond), summarize, RT.task1=mean(RT.task1, na.rm=T), ERR.task1=sum(ERR.task1, na.rm=T))
agg.item.task1 = ddply(dat,.(targ_ID, task.soa, cond), summarize, RT.task1=mean(RT.task1, na.rm=T), ERR.task1=sum(ERR.task1, na.rm=T))
agg.subj.task1$PCT.task1 = agg.subj.task1$ERR.task1 / length(levels(dat$targ_ID)) * 100
agg.item.task1$PCT.task1 = agg.item.task1$ERR.task1 / length(levels(dat$subj)) * 100

#-------------------------------------------------------------latency analyses----------------
#descriptive
with(agg.subj.task1,describeBy(RT.task1,group=list(task.soa, cond),mat=T))

#------------------------participant analysis
ezANOVA(data=agg.subj.task1, dv=.("RT.task1"), wid=.("subj"), within=.("task.soa", "cond"),detailed=T,type=3)

#-------------------------------item analysis
ezANOVA(data=agg.item.task1, dv=.("RT.task1"), wid=.("targ_ID"), within=.("task.soa", "cond"),detailed=T,type=3)

#--------------------------------------------------------------error analyses-----------------
#descriptive
with(agg.subj.task1,describeBy(PCT.task1,group=list(task.soa, cond),mat=T))

#------------------------participant analysis
ezANOVA(data=agg.subj.task1, dv=.("PCT.task1"), wid=.("subj"), within=.("task.soa", "cond"),detailed=T,type=3)

#-------------------------------item analysis
ezANOVA(data=agg.item.task1, dv=.("PCT.task1"), wid=.("targ_ID"), within=.("task.soa", "cond"),detailed=T,type=3)

#-----------------------------------------------TASK 2: sound naming---------------------------------#
#----------------------------ANOVA: cond x task.soa------------------------------------------------
#-------------aggregate---------
agg.subj.task2 = ddply(dat,.(subj, task.soa, cond), summarize, RT.task2=mean(RT.task2, na.rm=T), ERR.task2=sum(ERR.task2, na.rm=T))
agg.item.task2 = ddply(dat,.(targ_ID, task.soa, cond), summarize, RT.task2=mean(RT.task2, na.rm=T), ERR.task2=sum(ERR.task2, na.rm=T))
agg.subj.task2$PCT.task2 = agg.subj.task2$ERR.task2 / length(levels(dat$targ_ID)) * 100
agg.item.task2$PCT.task2 = agg.item.task2$ERR.task2 / length(levels(dat$subj)) * 100

#-------------------------------------------------------------latency analyses----------------
#descriptive
with(agg.subj.task2,describeBy(RT.task2,group=list(task.soa, cond),mat=T))

#------------------------participant analysis
ezANOVA(data=agg.subj.task2, dv=.("RT.task2"), wid=.("subj"), within=.("task.soa", "cond"),detailed=T,type=3)

#-------------------------------item analysis
ezANOVA(data=agg.item.task2, dv=.("RT.task2"), wid=.("targ_ID"), within=.("task.soa", "cond"),detailed=T,type=3)

#--------------------------------------------------------------error analyses-----------------
#descriptive
with(agg.subj.task2,describeBy(PCT.task2,group=list(task.soa, cond),mat=T))

#------------------------participant analysis
ezANOVA(data=agg.subj.task2, dv=.("PCT.task2"), wid=.("subj"), within=.("task.soa", "cond"),detailed=T,type=3)

#-------------------------------item analysis
ezANOVA(data=agg.item.task2, dv=.("PCT.task2"), wid=.("targ_ID"), within=.("task.soa", "cond"),detailed=T,type=3)

#----------------------------t-tests: cond------------------------------------------------
#-------------aggregate---------
agg.subj.task2_C = ddply(dat,.(subj, cond), summarize, RT.task2=mean(RT.task2, na.rm=T), ERR.task2=sum(ERR.task2, na.rm=T))
agg.item.task2_C = ddply(dat,.(targ_ID, cond), summarize, RT.task2=mean(RT.task2, na.rm=T), ERR.task2=sum(ERR.task2, na.rm=T))
agg.subj.task2_C$PCT.task2 = agg.subj.task2_C$ERR.task2 / ((length(levels(dat$targ_ID)))*2) * 100
agg.item.task2_C$PCT.task2 = agg.item.task2_C$ERR.task2 / ((length(levels(dat$subj)))*2) * 100

#reshape data - participants
agg.subj.task2.wide_C = longToWide (agg.subj.task2_C,
                                    RT.task2 + ERR.task2 + PCT.task2 ~ cond, sep = ".")

#reshape data -items
agg.item.task2.wide_C = longToWide (agg.item.task2_C,
                                    RT.task2 + ERR.task2 + PCT.task2 ~ cond, sep = ".")

#-------------------------------------------------------------latency analyses----------------
#descriptive
with(agg.subj.task2_C,describeBy(RT.task2,group=list(cond),mat=T))

#-------------------------------congruency facilitation effect

#------------------------participant analysis
res = t.test(agg.subj.task2.wide_C$RT.task2.congruent, agg.subj.task2.wide_C$RT.task2.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.task2.wide_C$RT.task2.congruent, agg.subj.task2.wide_C$RT.task2.unrelated, method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.task2.wide_C$RT.task2.congruent, agg.item.task2.wide_C$RT.task2.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.task2.wide_C$RT.task2.congruent, agg.item.task2.wide_C$RT.task2.unrelated, method = "paired")

#---------------------------------semantic interference effect

#------------------------participant analysis
res = t.test(agg.subj.task2.wide_C$RT.task2.semantic, agg.subj.task2.wide_C$RT.task2.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.task2.wide_C$RT.task2.semantic, agg.subj.task2.wide_C$RT.task2.unrelated, method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.task2.wide_C$RT.task2.semantic, agg.item.task2.wide_C$RT.task2.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.task2.wide_C$RT.task2.semantic, agg.item.task2.wide_C$RT.task2.unrelated, method = "paired")

#--------------------------------------------------------------error analyses-----------------
#descriptive
with(agg.subj.task2_C,describeBy(PCT.task2,group=list(cond),mat=T))

#-------------------------------congruency facilitation effect

#------------------------participant analysis
res = t.test(agg.subj.task2.wide_C$PCT.task2.congruent, agg.subj.task2.wide_C$PCT.task2.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.task2.wide_C$PCT.task2.congruent, agg.subj.task2.wide_C$PCT.task2.unrelated, method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.task2.wide_C$PCT.task2.congruent, agg.item.task2.wide_C$PCT.task2.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.task2.wide_C$PCT.task2.congruent, agg.item.task2.wide_C$PCT.task2.unrelated, method = "paired")

#---------------------------------semantic interference effect

#------------------------participant analysis
res = t.test(agg.subj.task2.wide_C$PCT.task2.semantic, agg.subj.task2.wide_C$PCT.task2.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.task2.wide_C$PCT.task2.semantic, agg.subj.task2.wide_C$PCT.task2.unrelated, method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.task2.wide_C$PCT.task2.semantic, agg.item.task2.wide_C$PCT.task2.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.task2.wide_C$PCT.task2.semantic, agg.item.task2.wide_C$PCT.task2.unrelated, method = "paired")

#----------------------------t-tests: cond x task.soa-----------------------------------------
#reshape data - participants
agg.subj.task2.wide = longToWide (agg.subj.task2,
                                  RT.task2 + ERR.task2 + PCT.task2 ~ task.soa | cond, sep = ".")

#reshape data -items
agg.item.task2.wide = longToWide (agg.item.task2,
                                  RT.task2 + ERR.task2 + PCT.task2 ~ task.soa | cond, sep = ".")

#-------------------------------------------------------------latency analyses----------------

#-------------------------------congruency facilitation effect
#task.soa = 0 ms

#------------------------participant analysis
res = t.test(agg.subj.task2.wide$'RT.task2.0.congruent', agg.subj.task2.wide$'RT.task2.0.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.task2.wide$'RT.task2.0.congruent', agg.subj.task2.wide$'RT.task2.0.unrelated', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.task2.wide$'RT.task2.0.congruent', agg.item.task2.wide$'RT.task2.0.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.task2.wide$'RT.task2.0.congruent', agg.item.task2.wide$'RT.task2.0.unrelated', method = "paired")

#task.soa = 1500 ms

#------------------------participant analysis
res = t.test(agg.subj.task2.wide$'RT.task2.1500.congruent', agg.subj.task2.wide$'RT.task2.1500.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.task2.wide$'RT.task2.1500.congruent', agg.subj.task2.wide$'RT.task2.1500.unrelated', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.task2.wide$'RT.task2.1500.congruent', agg.item.task2.wide$'RT.task2.1500.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.task2.wide$'RT.task2.1500.congruent', agg.item.task2.wide$'RT.task2.1500.unrelated', method = "paired")

#---------------------------------semantic interference effect
#task.SOA = 0 ms

#------------------------participant analysis
res = t.test(agg.subj.task2.wide$'RT.task2.0.semantic', agg.subj.task2.wide$'RT.task2.0.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.task2.wide$'RT.task2.0.semantic', agg.subj.task2.wide$'RT.task2.0.unrelated', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.task2.wide$'RT.task2.0.semantic', agg.item.task2.wide$'RT.task2.0.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.task2.wide$'RT.task2.0.semantic', agg.item.task2.wide$'RT.task2.0.unrelated', method = "paired")

#task.SOA = 1500 ms

#------------------------participant analysis
res = t.test(agg.subj.task2.wide$'RT.task2.1500.semantic', agg.subj.task2.wide$'RT.task2.1500.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.task2.wide$'RT.task2.1500.semantic', agg.subj.task2.wide$'RT.task2.1500.unrelated', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.task2.wide$'RT.task2.1500.semantic', agg.item.task2.wide$'RT.task2.1500.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.task2.wide$'RT.task2.1500.semantic', agg.item.task2.wide$'RT.task2.1500.unrelated', method = "paired")

#compute latency differences per task.soa
agg.subj.task2.wide$'diff_RT.con_unr.0000' = agg.subj.task2.wide$'RT.task2.0.congruent' - agg.subj.task2.wide$'RT.task2.0.unrelated'
agg.subj.task2.wide$'diff_RT.con_unr.1500' = agg.subj.task2.wide$'RT.task2.1500.congruent' - agg.subj.task2.wide$'RT.task2.1500.unrelated'
agg.subj.task2.wide$'diff_RT.sem_unr.0000' = agg.subj.task2.wide$'RT.task2.0.semantic' - agg.subj.task2.wide$'RT.task2.0.unrelated'
agg.subj.task2.wide$'diff_RT.sem_unr.1500' = agg.subj.task2.wide$'RT.task2.1500.semantic' - agg.subj.task2.wide$'RT.task2.1500.unrelated'
agg.item.task2.wide$'diff_RT.con_unr.0000' = agg.item.task2.wide$'RT.task2.0.congruent' - agg.item.task2.wide$'RT.task2.0.unrelated'
agg.item.task2.wide$'diff_RT.con_unr.1500' = agg.item.task2.wide$'RT.task2.1500.congruent' - agg.item.task2.wide$'RT.task2.1500.unrelated'
agg.item.task2.wide$'diff_RT.sem_unr.0000' = agg.item.task2.wide$'RT.task2.0.semantic' - agg.item.task2.wide$'RT.task2.0.unrelated'
agg.item.task2.wide$'diff_RT.sem_unr.1500' = agg.item.task2.wide$'RT.task2.1500.semantic' - agg.item.task2.wide$'RT.task2.1500.unrelated'

#----------------comparison of congruency facilitation effects between task.soa

#------------------------participant analysis
res = t.test(agg.subj.task2.wide$'diff_RT.con_unr.0000', agg.subj.task2.wide$'diff_RT.con_unr.1500', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.task2.wide$'diff_RT.con_unr.0000', agg.subj.task2.wide$'diff_RT.con_unr.1500', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.task2.wide$'diff_RT.con_unr.0000', agg.item.task2.wide$'diff_RT.con_unr.1500', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.task2.wide$'diff_RT.con_unr.0000', agg.item.task2.wide$'diff_RT.con_unr.1500', method = "paired")

#------------------comparison of semantic interference effects between task.soa

#------------------------participant analysis
res = t.test(agg.subj.task2.wide$'diff_RT.sem_unr.0000', agg.subj.task2.wide$'diff_RT.sem_unr.1500', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.task2.wide$'diff_RT.sem_unr.0000', agg.subj.task2.wide$'diff_RT.sem_unr.1500', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.task2.wide$'diff_RT.sem_unr.0000', agg.item.task2.wide$'diff_RT.sem_unr.1500', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.task2.wide$'diff_RT.sem_unr.0000', agg.item.task2.wide$'diff_RT.sem_unr.1500', method = "paired")

###################################
#save workspace
save.image(file="exp03_anova.RData")