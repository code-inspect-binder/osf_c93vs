#------------------------------------------------------------------------------#
#This file contains R-code for data analyses of Experiment 10 reported in:     #
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

setwd("C:/Users/Stefan/Downloads/osfstorage-archive/Experiment 10") #set to location of the data files

#------data analyses------#
#import data
#variant 1 (choose only one)
#dat = read.table(file = "exp10_data.txt", header = TRUE)
#dat$subj = as.factor(dat$subj)
#dat$soa = as.factor(dat$soa)

#variant 2 (choose only one)
load("exp10_prep.RData")

#variant 3 (choose only one)
#dat = read.table(file = "https://osf.io/j74qs/download", header = TRUE)
#dat$subj = as.factor(dat$subj)
#dat$soa = as.factor(dat$soa)

#data structure
str(dat)
#- targ_ID = target sound  ID
#- cond = distractor condition (congruent; unrelated)
#- soa = stimulus-onset-asynchrony (-200 ms; 0 ms; 200 ms)
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
#Greenhouse-Geisser correction - df
aov_ez(data=agg.item, dv=.("PCT"), id=.("targ_ID"), within=.("mod", "soa", "cond"))

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

#-------------------------------soa -200 ms vs soa 0 ms

#------------------------participant analysis
#note: Bonferroni correction: p*n (n-number of tests; n = 3)
res = t.test(agg.subj.wide_S$'RT.-200', agg.subj.wide_S$'RT.0', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_S$'RT.-200', agg.subj.wide_S$'RT.0', method = "paired")

#-------------------------------item analysis
#note: Bonferroni correction: p*n (n-number of tests; n = 3)
res = t.test(agg.item.wide_S$'RT.-200', agg.item.wide_S$'RT.0', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_S$'RT.-200', agg.item.wide_S$'RT.0', method = "paired")

#-------------------------------soa -200 ms vs soa 200 ms

#------------------------participant analysis
#note: for Bonferroni correction: p*n (n-number of tests; n = 3)
res = t.test(agg.subj.wide_S$'RT.-200', agg.subj.wide_S$'RT.200', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_S$'RT.-200', agg.subj.wide_S$'RT.200', method = "paired")

#-------------------------------item analysis
#note: for Bonferroni correction: p*n (n-number of tests; n = 3)
res = t.test(agg.item.wide_S$'RT.-200', agg.item.wide_S$'RT.200', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_S$'RT.-200', agg.item.wide_S$'RT.200', method = "paired")

#-------------------------------soa 0 ms vs soa 200 ms

#------------------------participant analysis
#note: for Bonferroni correction: p*n (n-number of tests; n = 3)
res = t.test(agg.subj.wide_S$'RT.0', agg.subj.wide_S$'RT.200', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_S$'RT.0', agg.subj.wide_S$'RT.200', method = "paired")

#-------------------------------item analysis
#note: Bonferroni correction: p*n (n-number of tests; n = 3)
res = t.test(agg.item.wide_S$'RT.0', agg.item.wide_S$'RT.200', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_S$'RT.0', agg.item.wide_S$'RT.200', method = "paired")

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

#compute latency differences per mod
agg.subj.wide_CM$diff_RT.con_unr.vis = agg.subj.wide_CM$RT.visual.congruent - agg.subj.wide_CM$RT.visual.unrelated
agg.subj.wide_CM$diff_RT.con_unr.aud = agg.subj.wide_CM$RT.auditory.congruent - agg.subj.wide_CM$RT.auditory.unrelated
agg.item.wide_CM$diff_RT.con_unr.vis = agg.item.wide_CM$RT.visual.congruent - agg.item.wide_CM$RT.visual.unrelated
agg.item.wide_CM$diff_RT.con_unr.aud = agg.item.wide_CM$RT.auditory.congruent - agg.item.wide_CM$RT.auditory.unrelated

#----------------comparison of congruency facilitation effects between mod

#------------------------participant analysis
res = t.test(agg.subj.wide_CM$diff_RT.con_unr.vis, agg.subj.wide_CM$diff_RT.con_unr.aud, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_CM$diff_RT.con_unr.vis, agg.subj.wide_CM$diff_RT.con_unr.aud, method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_CM$diff_RT.con_unr.vis, agg.item.wide_CM$diff_RT.con_unr.aud, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_CM$diff_RT.con_unr.vis, agg.item.wide_CM$diff_RT.con_unr.aud, method = "paired")

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
#soa = -200 ms

#------------------------participant analysis
res = t.test(agg.subj.wide_CS$'RT.-200.congruent', agg.subj.wide_CS$'RT.-200.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_CS$'RT.-200.congruent', agg.subj.wide_CS$'RT.-200.unrelated', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_CS$'RT.-200.congruent', agg.item.wide_CS$'RT.-200.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_CS$'RT.-200.congruent', agg.item.wide_CS$'RT.-200.unrelated', method = "paired")

#soa = 0 ms

#------------------------participant analysis
res = t.test(agg.subj.wide_CS$'RT.0.congruent', agg.subj.wide_CS$'RT.0.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_CS$'RT.0.congruent', agg.subj.wide_CS$'RT.0.unrelated', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_CS$'RT.0.congruent', agg.item.wide_CS$'RT.0.unrelated', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_CS$'RT.0.congruent', agg.item.wide_CS$'RT.0.unrelated', method = "paired")

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

#compute latency differences per soa
agg.subj.wide_CS$'diff_RT.con_unr.-200' = agg.subj.wide_CS$'RT.-200.congruent' - agg.subj.wide_CS$'RT.-200.unrelated'
agg.subj.wide_CS$'diff_RT.con_unr.0' = agg.subj.wide_CS$'RT.0.congruent' - agg.subj.wide_CS$'RT.0.unrelated'
agg.subj.wide_CS$'diff_RT.con_unr.200' = agg.subj.wide_CS$'RT.200.congruent' - agg.subj.wide_CS$'RT.200.unrelated'
agg.item.wide_CS$'diff_RT.con_unr.-200' = agg.item.wide_CS$'RT.-200.congruent' - agg.item.wide_CS$'RT.-200.unrelated'
agg.item.wide_CS$'diff_RT.con_unr.0' = agg.item.wide_CS$'RT.0.congruent' - agg.item.wide_CS$'RT.0.unrelated'
agg.item.wide_CS$'diff_RT.con_unr.200' = agg.item.wide_CS$'RT.200.congruent' - agg.item.wide_CS$'RT.200.unrelated'

#----------------comparison of congruency facilitation effects between soa
#-------------------------------soa -200 ms vs soa 0 ms

#------------------------participant analysis
res = t.test(agg.subj.wide_CS$'diff_RT.con_unr.-200', agg.subj.wide_CS$'diff_RT.con_unr.0', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_CS$'diff_RT.con_unr.-200', agg.subj.wide_CS$'diff_RT.con_unr.0', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_CS$'diff_RT.con_unr.-200', agg.item.wide_CS$'diff_RT.con_unr.0', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_CS$'diff_RT.con_unr.-200', agg.item.wide_CS$'diff_RT.con_unr.0', method = "paired")

#-------------------------------soa -200 ms vs soa 200 ms

#------------------------participant analysis
res = t.test(agg.subj.wide_CS$'diff_RT.con_unr.-200', agg.subj.wide_CS$'diff_RT.con_unr.200', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_CS$'diff_RT.con_unr.-200', agg.subj.wide_CS$'diff_RT.con_unr.200', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_CS$'diff_RT.con_unr.-200', agg.item.wide_CS$'diff_RT.con_unr.200', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_CS$'diff_RT.con_unr.-200', agg.item.wide_CS$'diff_RT.con_unr.200', method = "paired")

#-------------------------------soa 0 ms vs soa 200 ms

#------------------------participant analysis
res = t.test(agg.subj.wide_CS$'diff_RT.con_unr.0', agg.subj.wide_CS$'diff_RT.con_unr.200', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_CS$'diff_RT.con_unr.0', agg.subj.wide_CS$'diff_RT.con_unr.200', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_CS$'diff_RT.con_unr.0', agg.item.wide_CS$'diff_RT.con_unr.200', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_CS$'diff_RT.con_unr.0', agg.item.wide_CS$'diff_RT.con_unr.200', method = "paired")

#----------------------------t-tests: mod x soa------------------------------------------------
#-------------aggregate---------
agg.subj_MS = ddply(dat,.(subj, mod, soa), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.item_MS = ddply(dat,.(targ_ID, mod, soa), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.subj_MS$PCT = agg.subj_MS$ERR / (length(levels(dat$targ_ID))*2) * 100
agg.item_MS$PCT = agg.item_MS$ERR / (length(levels(dat$subj))*2) * 100

#reshape data - participants
agg.subj.wide_MS = longToWide (agg.subj_MS,
                               RT + ERR + PCT ~ soa | mod, sep = ".")

#reshape data - items
agg.item.wide_MS = longToWide (agg.item_MS,
                               RT + ERR + PCT ~ soa | mod, sep = ".")

#-------------------------------------------------------------latency analyses----------------
#descriptive
with(agg.subj_MS,describeBy(RT,group=list(mod, soa),mat=T))

#-------------------------------visual vs auditory distractor words
#soa = -200 ms

#------------------------participant analysis
res = t.test(agg.subj.wide_MS$'RT.-200.visual', agg.subj.wide_MS$'RT.-200.auditory', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_MS$'RT.-200.visual', agg.subj.wide_MS$'RT.-200.auditory', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_MS$'RT.-200.visual', agg.item.wide_MS$'RT.-200.auditory', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_MS$'RT.-200.visual', agg.item.wide_MS$'RT.-200.auditory', method = "paired")

#soa = 0 ms

#------------------------participant analysis
res = t.test(agg.subj.wide_MS$'RT.0.visual', agg.subj.wide_MS$'RT.0.auditory', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_MS$'RT.0.visual', agg.subj.wide_MS$'RT.0.auditory', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_MS$'RT.0.visual', agg.item.wide_MS$'RT.0.auditory', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_MS$'RT.0.visual', agg.item.wide_MS$'RT.0.auditory', method = "paired")

#soa = 200 ms

#------------------------participant analysis
res = t.test(agg.subj.wide_MS$'RT.200.visual', agg.subj.wide_MS$'RT.200.auditory', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide_MS$'RT.200.visual', agg.subj.wide_MS$'RT.200.auditory', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide_MS$'RT.200.visual', agg.item.wide_MS$'RT.200.auditory', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide_MS$'RT.200.visual', agg.item.wide_MS$'RT.200.auditory', method = "paired")

###################################
#save workspace
save.image(file="exp10_anova.RData")