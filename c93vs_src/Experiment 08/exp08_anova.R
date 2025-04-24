#------------------------------------------------------------------------------#
#This file contains R-code for data analyses of Experiment 8 reported in:      #
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

setwd("C:/Users/Stefan/Downloads/osfstorage-archive/Experiment 08") #set to location of the data files

#------data analyses------#
#import data
#variant 1 (choose only one)
#dat = read.table(file = "exp08_data.txt", header = TRUE)
#dat$subj = as.factor(dat$subj)

#variant 2 (choose only one)
load("exp08_prep.RData")

#variant 3 (choose only one)
#dat = read.table(file = "https://osf.io/y46av/download", header = TRUE)
#dat$subj = as.factor(dat$subj)

#data structure
str(dat)
#- targ_ID = target picture  ID
#- cond = distractor condition (semantic_form; unrelated_noform; semantic_noform)
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

#----------------------------ANOVA: cond ------------------------------------------------
#-------------aggregate---------
agg.subj = ddply(dat,.(subj, cond), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.item = ddply(dat,.(targ_ID, cond), summarize, RT=mean(RT, na.rm=T), ERR=sum(ERR, na.rm=T))
agg.subj$PCT = agg.subj$ERR / (length(levels(dat$targ_ID))*2) * 100
agg.item$PCT = agg.item$ERR / (length(levels(dat$subj))*2) * 100

#-------------------------------------------------------------latency analyses----------------
#descriptive
with(agg.subj,describeBy(RT,group=list(cond),mat=T))

#------------------------participant analysis
ezANOVA(data=agg.subj, dv=.("RT"), wid=.("subj"), within=.("cond"),detailed=T,type=3)

#-------------------------------item analysis
ezANOVA(data=agg.item, dv=.("RT"), wid=.("targ_ID"), within=.("cond"),detailed=T,type=3)
#Greenhouse-Geisser correction - df
aov_ez(data=agg.item, dv=.("RT"), id=.("targ_ID"), within=.("cond"))

#--------------------------------------------------------------error analyses-----------------
#descriptive
with(agg.subj,describeBy(PCT,group=list(cond),mat=T))

#------------------------participant analysis
ezANOVA(data=agg.subj, dv=.("PCT"), wid=.("subj"), within=.("cond"),detailed=T,type=3)

#-------------------------------item analysis
ezANOVA(data=agg.item, dv=.("PCT"), wid=.("targ_ID"), within=.("cond"),detailed=T,type=3)

#----------------------------t-tests: cond------------------------------------------------
#reshape data - participants
agg.subj.wide = longToWide (agg.subj,
                              RT + ERR + PCT ~ cond, sep = ".")

#reshape data - items
agg.item.wide = longToWide (agg.item,
                              RT + ERR + PCT ~ cond, sep = ".")

#-------------------------------------------------------------latency analyses----------------

#---------------------------------semantic interference effect---form

#------------------------participant analysis
res = t.test(agg.subj.wide$RT.semantic_form, agg.subj.wide$RT.unrelated_noform, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide$RT.semantic_form, agg.subj.wide$RT.unrelated_noform, method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide$RT.semantic_form, agg.item.wide$RT.unrelated_noform, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide$RT.semantic_form, agg.item.wide$RT.unrelated_noform, method = "paired")

#---------------------------------semantic interference effect---noform

#------------------------participant analysis
res = t.test(agg.subj.wide$RT.semantic_noform, agg.subj.wide$RT.unrelated_noform, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide$RT.semantic_noform, agg.subj.wide$RT.unrelated_noform, method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide$RT.semantic_noform, agg.item.wide$RT.unrelated_noform, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide$RT.semantic_noform, agg.item.wide$RT.unrelated_noform, method = "paired")

#compute latency differences of semantic interference effects 
agg.subj.wide$'diff_RT.sem_unr.form' = agg.subj.wide$'RT.semantic_form' - agg.subj.wide$'RT.unrelated_noform'
agg.subj.wide$'diff_RT.sem_unr.noform' = agg.subj.wide$'RT.semantic_noform' - agg.subj.wide$'RT.unrelated_noform'
agg.item.wide$'diff_RT.sem_unr.form' = agg.item.wide$'RT.semantic_form' - agg.item.wide$'RT.unrelated_noform'
agg.item.wide$'diff_RT.sem_unr.noform' = agg.item.wide$'RT.semantic_noform' - agg.item.wide$'RT.unrelated_noform'

#------------------comparison of semantic interference effects (form vs. noform)

#------------------------participant analysis
res = t.test(agg.subj.wide$'diff_RT.sem_unr.form', agg.subj.wide$'diff_RT.sem_unr.noform', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide$'diff_RT.sem_unr.form', agg.subj.wide$'diff_RT.sem_unr.noform', method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide$'diff_RT.sem_unr.form', agg.item.wide$'diff_RT.sem_unr.noform', paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide$'diff_RT.sem_unr.form', agg.item.wide$'diff_RT.sem_unr.noform', method = "paired")

###################################
##################################################
#preparation for correlation of RT and rating differences (semantic vs. unrelated)

#remove not required variables
exp08_RT.subj = agg.subj.wide[ ,c("subj",
                             "diff_RT.sem_unr.form", "diff_RT.sem_unr.noform")]
exp08_RT.item = agg.item.wide[ ,c("targ_ID",
                             "diff_RT.sem_unr.form", "diff_RT.sem_unr.noform")]

#save data
write.table(exp08_RT.subj, "exp08_RT.subj.txt", sep = ",", col.names = T)
write.table(exp08_RT.item, "exp08_RT.item.txt", sep = ",", col.names = T)

###################################
#save workspace
save.image(file="exp08_anova.RData")