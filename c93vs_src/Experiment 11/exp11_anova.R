#------------------------------------------------------------------------------#
#This file contains R-code for data analyses of Experiment 11 reported in:     #
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

setwd("C:/Users/Stefan/Downloads/osfstorage-archive/Experiment 11") #set to location of the data files

#------data analyses------#
#import data
#variant 1 (choose only one)
#dat = read.table(file = "exp11_data.txt", header = TRUE)
#dat$subj = as.factor(dat$subj)
#dat$soa = as.factor(dat$soa)

#variant 2 (choose only one)
load("exp11_prep.RData")

#variant 3 (choose only one)
#dat = read.table(file = "https://osf.io/hybvj/download", header = TRUE)
#dat$subj = as.factor(dat$subj)
#dat$soa = as.factor(dat$soa)

#data structure
str(dat)
#- targ_ID = target sound  ID
#- cond = distractor condition (congruent; unrelated)
#- soa = stimulus-onset-asynchrony (200 ms; 400 ms; 600 ms)
#- mod = distractor modality (visual, auditory)
#- subj = participant ID
#- trial = trial number
#- RT = naming latency
#- ERR.code = error code
#- dist_ID = distractor word ID
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

#----------------------------ANOVA: soa x cond x mod------------------------------------------------
#-------------aggregate---------
agg.subj = ddply(dat,.(subj, soa, cond, mod), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.item = ddply(dat,.(targ_ID, soa, cond, mod), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.subj$PCT = agg.subj$ERR / length(levels(dat$targ_ID)) * 100
agg.item$PCT = agg.item$ERR / length(levels(dat$subj)) * 100

#-------------------------------------------------------------latency analyses----------------
#descriptive
with(agg.subj,describeBy(RT,group=list(soa, cond, mod),mat=T))

#------------------------participant analysis
ezANOVA(data=agg.subj, dv=.("RT"), wid=.("subj"), within=.("mod", "soa", "cond"),detailed=T,type=3)
#Greenhouse-Geisser correction - df
aov_ez(data=agg.subj, dv=.("RT"), id=.("subj"), within=.("mod", "soa", "cond"))

#-------------------------------item analysis
ezANOVA(data=agg.item, dv=.("RT"), wid=.("targ_ID"), within=.("mod", "soa", "cond"),detailed=T,type=3)
#Greenhouse-Geisser correction - df
aov_ez(data=agg.item, dv=.("RT"), id=.("targ_ID"), within=.("mod", "soa", "cond"))

#--------------------------------------------------------------error analyses-----------------
#descriptive
with(agg.subj,describeBy(PCT,group=list(soa, cond, mod),mat=T))

#------------------------participant analysis
ezANOVA(data=agg.subj, dv=.("PCT"), wid=.("subj"), within=.("mod", "soa", "cond"),detailed=T,type=3)

#-------------------------------item analysis
ezANOVA(data=agg.item, dv=.("PCT"), wid=.("targ_ID"), within=.("mod", "soa", "cond"),detailed=T,type=3)

#----------------------------t-tests: soa------------------------------------------------
#-------------aggregate---------
agg.subj_S = ddply(dat,.(subj, soa), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.item_S = ddply(dat,.(targ_ID, soa), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.subj_S$PCT = agg.subj_S$ERR / (length(levels(dat$targ_ID))*4) * 100
agg.item_S$PCT = agg.item_S$ERR / (length(levels(dat$subj))*4) * 100

#reshape data - participants
agg.subj.wide_S = longToWide (agg.subj_S,
                              RT + ERR + PCT ~ soa, sep = ".")

#reshape data - items
agg.item.wide_S = longToWide (agg.item_S,
                              RT + ERR + PCT ~ soa, sep = ".")

#-------------------------------------------------------------latency analyses----------------
#descriptive
with(agg.subj_S,describeBy(RT,group=list(soa),mat=T))

#-------------------------------soa 200 ms vs soa 400 ms

#------------------------participant analysis
#note: Bonferroni correction: p*n (n-number of tests; n = 3)
res = t.test(agg.subj.wide_S$'RT.200', agg.subj.wide_S$'RT.400', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_S$'RT.200', agg.subj.wide_S$'RT.400', method = "paired")

#-------------------------------item analysis
#note: Bonferroni correction: p*n (n-number of tests; n = 3)
res = t.test(agg.item.wide_S$'RT.200', agg.item.wide_S$'RT.400', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_S$'RT.200', agg.item.wide_S$'RT.400', method = "paired")

#-------------------------------soa 200 ms vs soa 600 ms

#------------------------participant analysis
#note: for Bonferroni correction: p*n (n-number of tests; n = 3)
res = t.test(agg.subj.wide_S$'RT.200', agg.subj.wide_S$'RT.600', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_S$'RT.200', agg.subj.wide_S$'RT.600', method = "paired")

#-------------------------------item analysis
#note: for Bonferroni correction: p*n (n-number of tests; n = 3)
res = t.test(agg.item.wide_S$'RT.200', agg.item.wide_S$'RT.600', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_S$'RT.200', agg.item.wide_S$'RT.600', method = "paired")

#-------------------------------soa 400 ms vs soa 600 ms

#------------------------participant analysis
#note: for Bonferroni correction: p*n (n-number of tests; n = 3)
res = t.test(agg.subj.wide_S$'RT.400', agg.subj.wide_S$'RT.600', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_S$'RT.400', agg.subj.wide_S$'RT.600', method = "paired")

#-------------------------------item analysis
#note: Bonferroni correction: p*n (n-number of tests; n = 3)
res = t.test(agg.item.wide_S$'RT.400', agg.item.wide_S$'RT.600', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_S$'RT.400', agg.item.wide_S$'RT.600', method = "paired")

#----------------------------t-tests: cond x mod------------------------------------------------
#-------------aggregate---------
agg.subj_CM = ddply(dat,.(subj, cond, mod), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.item_CM = ddply(dat,.(targ_ID, cond, mod), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.subj_CM$PCT = agg.subj_CM$ERR / (length(levels(dat$targ_ID))*3) * 100
agg.item_CM$PCT = agg.item_CM$ERR / (length(levels(dat$subj))*3) * 100

#reshape data - participants
agg.subj.wide_CM = longToWide (agg.subj_CM,
                              RT + ERR + PCT ~ mod | cond, sep = ".")

#reshape data - items
agg.item.wide_CM = longToWide (agg.item_CM,
                              RT + ERR + PCT ~ mod | cond, sep = ".")

#-------------------------------------------------------------latency analyses----------------
#descriptive
with(agg.subj_CM,describeBy(RT,group=list(cond, mod),mat=T))

#-------------------------------congruency facilitation effect
#visual distractor words

#------------------------participant analysis
res = t.test(agg.subj.wide_CM$RT.visual.congruent, agg.subj.wide_CM$RT.visual.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_CM$RT.visual.congruent, agg.subj.wide_CM$RT.visual.unrelated, method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_CM$RT.visual.congruent, agg.item.wide_CM$RT.visual.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_CM$RT.visual.congruent, agg.item.wide_CM$RT.visual.unrelated, method = "paired")

#auditory distractor words

#------------------------participant analysis
res = t.test(agg.subj.wide_CM$RT.auditory.congruent, agg.subj.wide_CM$RT.auditory.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_CM$RT.auditory.congruent, agg.subj.wide_CM$RT.auditory.unrelated, method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_CM$RT.auditory.congruent, agg.item.wide_CM$RT.auditory.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_CM$RT.auditory.congruent, agg.item.wide_CM$RT.auditory.unrelated, method = "paired")

#----------------------------t-tests: cond x soa------------------------------------------------
#-------------aggregate---------
agg.subj_CS = ddply(dat,.(subj, cond, soa), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.item_CS = ddply(dat,.(targ_ID, cond, soa), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.subj_CS$PCT = agg.subj_CS$ERR / (length(levels(dat$targ_ID))*2) * 100
agg.item_CS$PCT = agg.item_CS$ERR / (length(levels(dat$subj))*2) * 100

#reshape data - participants
agg.subj.wide_CS = longToWide (agg.subj_CS,
                               RT + ERR + PCT ~ soa | cond, sep = ".")

#reshape data - items
agg.item.wide_CS = longToWide (agg.item_CS,
                               RT + ERR + PCT ~ soa | cond, sep = ".")

#-------------------------------------------------------------latency analyses----------------
#descriptive
with(agg.subj_CS,describeBy(RT,group=list(cond, soa),mat=T))

#-------------------------------congruency facilitation effect
#soa = 200 ms

#------------------------participant analysis
res = t.test(agg.subj.wide_CS$'RT.200.congruent', agg.subj.wide_CS$'RT.200.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_CS$'RT.200.congruent', agg.subj.wide_CS$'RT.200.unrelated', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_CS$'RT.200.congruent', agg.item.wide_CS$'RT.200.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_CS$'RT.200.congruent', agg.item.wide_CS$'RT.200.unrelated', method = "paired")

#soa = 400 ms

#------------------------participant analysis
res = t.test(agg.subj.wide_CS$'RT.400.congruent', agg.subj.wide_CS$'RT.400.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_CS$'RT.400.congruent', agg.subj.wide_CS$'RT.400.unrelated', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_CS$'RT.400.congruent', agg.item.wide_CS$'RT.400.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_CS$'RT.400.congruent', agg.item.wide_CS$'RT.400.unrelated', method = "paired")

#soa = 600 ms

#------------------------participant analysis
res = t.test(agg.subj.wide_CS$'RT.600.congruent', agg.subj.wide_CS$'RT.600.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_CS$'RT.600.congruent', agg.subj.wide_CS$'RT.600.unrelated', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_CS$'RT.600.congruent', agg.item.wide_CS$'RT.600.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_CS$'RT.600.congruent', agg.item.wide_CS$'RT.600.unrelated', method = "paired")

###################################
##################################################
#Additional analyses (with special focus on soa = 200 ms, which was also tested in Exps 09 & 10)

#select soa 200 ms
agg.subj.200 = agg.subj[agg.subj$soa=='200',]
agg.subj.200=droplevels(agg.subj.200)
agg.item.200 = agg.item[agg.item$soa=='200',]
agg.item.200=droplevels(agg.item.200)

#-------------------------------------------------------------latency analyses----------------
#descriptive
with(agg.subj.200,describeBy(RT,group=list(cond, mod),mat=T))

#------------------------participant analysis
ezANOVA(data=agg.subj.200, dv=.("RT"), wid=.("subj"), within=.("mod", "cond"),detailed=T,type=3)

#-------------------------------item analysis
ezANOVA(data=agg.item.200, dv=.("RT"), wid=.("targ_ID"), within=.("mod", "cond"),detailed=T,type=3)

#--------------------------------------------------------------error analyses-----------------
#descriptive
with(agg.subj.200,describeBy(PCT,group=list(cond, mod),mat=T))

#------------------------participant analysis
ezANOVA(data=agg.subj.200, dv=.("PCT"), wid=.("subj"), within=.("mod", "cond"),detailed=T,type=3)

#-------------------------------item analysis
ezANOVA(data=agg.item.200, dv=.("PCT"), wid=.("targ_ID"), within=.("mod", "cond"),detailed=T,type=3)

#----------------------------t-tests: mod x cond x soa------------------------------------------------
#reshape data - participants
agg.subj.wide.200 = longToWide (agg.subj.200,
                               RT + ERR + PCT ~ mod | cond | soa, sep = ".")

#reshape data - items
agg.item.wide.200 = longToWide (agg.item.200,
                               RT + ERR + PCT ~ mod | cond | soa, sep = ".")

#-------------------------------------------------------------latency analyses----------------

#-------------------------------congruency facilitation effect
#visual distractor words
#soa = 200 ms

#------------------------participant analysis
res = t.test(agg.subj.wide.200$'RT.visual.congruent.200', agg.subj.wide.200$'RT.visual.unrelated.200', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide.200$'RT.visual.congruent.200', agg.subj.wide.200$'RT.visual.unrelated.200', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide.200$'RT.visual.congruent.200', agg.item.wide.200$'RT.visual.unrelated.200', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide.200$'RT.visual.congruent.200', agg.item.wide.200$'RT.visual.unrelated.200', method = "paired")

#auditory distractor words
#soa = 200 ms

#------------------------participant analysis
res = t.test(agg.subj.wide.200$'RT.auditory.congruent.200', agg.subj.wide.200$'RT.auditory.unrelated.200', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide.200$'RT.auditory.congruent.200', agg.subj.wide.200$'RT.auditory.unrelated.200', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide.200$'RT.auditory.congruent.200', agg.item.wide.200$'RT.auditory.unrelated.200', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide.200$'RT.auditory.congruent.200', agg.item.wide.200$'RT.auditory.unrelated.200', method = "paired")

#compute latency differences per mod
agg.subj.wide.200$'diff_RT.con_unr.200.vis' = agg.subj.wide.200$'RT.visual.congruent.200' - agg.subj.wide.200$'RT.visual.unrelated.200'
agg.subj.wide.200$'diff_RT.con_unr.200.aud' = agg.subj.wide.200$'RT.auditory.congruent.200' - agg.subj.wide.200$'RT.auditory.unrelated.200'
agg.item.wide.200$'diff_RT.con_unr.200.vis' = agg.item.wide.200$'RT.visual.congruent.200' - agg.item.wide.200$'RT.visual.unrelated.200'
agg.item.wide.200$'diff_RT.con_unr.200.aud' = agg.item.wide.200$'RT.auditory.congruent.200' - agg.item.wide.200$'RT.auditory.unrelated.200'

#----------------comparison of congruency facilitation effects between mod

#------------------------participant analysis
res = t.test(agg.subj.wide.200$'diff_RT.con_unr.200.vis', agg.subj.wide.200$'diff_RT.con_unr.200.aud', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide.200$'diff_RT.con_unr.200.vis', agg.subj.wide.200$'diff_RT.con_unr.200.aud', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide.200$'diff_RT.con_unr.200.vis', agg.item.wide.200$'diff_RT.con_unr.200.aud', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide.200$'diff_RT.con_unr.200.vis', agg.item.wide.200$'diff_RT.con_unr.200.aud', method = "paired")

###################################
#save workspace
save.image(file="exp11_anova.RData")