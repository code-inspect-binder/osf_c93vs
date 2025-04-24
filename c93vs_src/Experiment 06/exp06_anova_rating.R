#------------------------------------------------------------------------------#
#This file contains R-code for data analyses of Experiment 6 reported in:      #
# Wöhner, S. (2018).                                                           #
#    #Natürliche Geräusche und Bilder in Benennungsaufgaben -                  #
#     Semantische Kontexteffekte innerhalb und zwischen  Stimulusmodalitäten#  #
#                                                                              #
#(c) Stefan Wöhner (stefan.woehner@uni-leipzig.de)                             #
#------------------------------------------------------------------------------

#------Setup-------#
rm(list = ls())
library(ez)
library(afex)
library(plyr)
library(lattice)
library(psych)
library(MASS)
library(lsr)
library(ggpubr)
options(scipen=999)

setwd("C:/Users/Stefan/Downloads/osfstorage-archive/Experiment 06") #set to location of the data files

#------data analyses------#
#import data
#variant 1 (choose only one)
#dat = read.table(file = "exp06_data_rating.txt", header = TRUE)
#dat$subj = as.factor(dat$subj)

#variant 2 (choose only one)
load("exp06_prep_rating.RData")

#variant 3 (choose only one)
#dat = read.table(file = "https://osf.io/85f2e/download", header = TRUE)
#dat$subj = as.factor(dat$subj)

#data structure
str(dat)
#- subj = participant ID
#- trial = trial number
#- rating = rating score (1: target poorly recognizable; 5: target well recognizable)
#- cond = distractor condition (congruent; semantic; unrelated)
#- targ_ID = target picture  ID
#- dist_ID = distractor picture ID
#- exp = experiment number

#----------------------------ANOVA: cond------------------------------------------------
#-------------aggregate---------
agg.subj = ddply(dat,.(subj, cond), summarize, rat=mean(rating, na.rm=T))
agg.item = ddply(dat,.(targ_ID, cond), summarize, rat=mean(rating, na.rm=T))

#-------------------------------------------------------------rating analyses-------------
#descriptive
with(agg.subj,describeBy(rat,group=list(cond),mat=T))

#------------------------participant analysis
ezANOVA(data=agg.subj, dv=.("rat"), wid=.("subj"), within=.("cond"),detailed=T,type=3)
#Greenhouse-Geisser correction - df
aov_ez(data=agg.subj, dv=.("rat"), id=.("subj"), within=.("cond"))

#-------------------------------item analysis
ezANOVA(data=agg.item, dv=.("rat"), wid=.("targ_ID"), within=.("cond"),detailed=T,type=3)

#----------------------------t-tests: cond------------------------------------------------
#reshape data - participants
agg.subj.wide = longToWide (agg.subj,
                              rat ~ cond, sep = ".")

#reshape data - items
agg.item.wide = longToWide (agg.item,
                              rat ~ cond, sep = ".")

#---------------------------------------congruent vs. unrelated

#------------------------participant analysis
res = t.test(agg.subj.wide$rat.congruent, agg.subj.wide$rat.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide$rat.congruent, agg.subj.wide$rat.unrelated, method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide$rat.congruent, agg.item.wide$rat.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide$rat.congruent, agg.item.wide$rat.unrelated, method = "paired")

#----------------------------------------semantic vs. unrelated

#------------------------participant analysis
res = t.test(agg.subj.wide$rat.semantic, agg.subj.wide$rat.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide$rat.semantic, agg.subj.wide$rat.unrelated, method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide$rat.semantic, agg.item.wide$rat.unrelated, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide$rat.semantic, agg.item.wide$rat.unrelated, method = "paired")

###################################
##################################################
#preparation for correlation of RT and rating differences (semantic vs. unrelated)

#compute rating differences
agg.subj.wide$diff_rat.sem_unr=agg.subj.wide$rat.semantic-agg.subj.wide$rat.unrelated
agg.item.wide$diff_rat.sem_unr=agg.item.wide$rat.semantic-agg.item.wide$rat.unrelated

#import RT data
dat.subj = read.table(file = "exp06_RT.subj.txt", header = TRUE, sep = ",", check.names = FALSE)
dat.item = read.table(file = "exp06_RT.item.txt", header = TRUE, sep = ",", check.names = FALSE)

#combine data sets
agg.subj.wide.all = merge(agg.subj.wide, dat.subj, by="subj") 
agg.item.wide.all = merge(agg.item.wide, dat.item, by="targ_ID") 

#-------------------------------------------------------------correlation----------------

#----------------------------------------semantic vs. unrelated
#RT ~ rating
#soa = -200 ms (RT)

#------------------------participant analysis
cor.test(agg.subj.wide.all$'diff_RT.sem_unr.-200', agg.subj.wide.all$diff_rat.sem_unr)

#plot
#note: renaming "diff_RT.sem_unr.-200" into "diff_RT.sem_unr.m200" is necessary as ggcatter cannot handle "-" within column names
names(agg.subj.wide.all)[names(agg.subj.wide.all) == 'diff_RT.sem_unr.-200'] = 'diff_RT.sem_unr.m200'

ggscatter(agg.subj.wide.all, x = "diff_rat.sem_unr", y = "diff_RT.sem_unr.m200", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean Rating_diff", ylab = "Mean RT_diff")

#-------------------------------item analysis
cor.test(agg.item.wide.all$'diff_RT.sem_unr.-200', agg.item.wide.all$diff_rat.sem_unr)

#plot
#note: renaming "diff_RT.sem_unr.-200" into "diff_RT.sem_unr.m200" is necessary as ggcatter cannot handle "-" within column names
names(agg.item.wide.all)[names(agg.item.wide.all) == 'diff_RT.sem_unr.-200'] = 'diff_RT.sem_unr.m200'

ggscatter(agg.item.wide.all, x = "diff_rat.sem_unr", y = "diff_RT.sem_unr.m200", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean Rating_diff", ylab = "Mean RT_diff")

#RT ~ rating
#soa = 0 ms (RT)

#------------------------participant analysis
cor.test(agg.subj.wide.all$'diff_RT.sem_unr.0000', agg.subj.wide.all$diff_rat.sem_unr)

#plot
ggscatter(agg.subj.wide.all, x = "diff_rat.sem_unr", y = "diff_RT.sem_unr.0000", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean Rating_diff", ylab = "Mean RT_diff")

#-------------------------------item analysis
cor.test(agg.item.wide.all$'diff_RT.sem_unr.0000', agg.item.wide.all$diff_rat.sem_unr)

#plot
ggscatter(agg.item.wide.all, x = "diff_rat.sem_unr", y = "diff_RT.sem_unr.0000", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean Rating_diff", ylab = "Mean RT_diff")

#RT ~ rating
#over soa

#------------------------participant analysis
cor.test(agg.subj.wide.all$diff_RT.sem_unr, agg.subj.wide.all$diff_rat.sem_unr)

#plot
ggscatter(agg.subj.wide.all, x = "diff_rat.sem_unr", y = "diff_RT.sem_unr", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean Rating_diff", ylab = "Mean RT_diff")

#-------------------------------item analysis
cor.test(agg.item.wide.all$diff_RT.sem_unr, agg.item.wide.all$diff_rat.sem_unr)

#plot
ggscatter(agg.item.wide.all, x = "diff_rat.sem_unr", y = "diff_RT.sem_unr", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean Rating_diff", ylab = "Mean RT_diff")

###################################
#save workspace
save.image(file="exp06_anova_rating.RData")
