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
library(ez)
library(afex)
library(plyr)
library(lattice)
library(psych)
library(MASS)
library(lsr)
library(ggpubr)
options(scipen=999)

setwd("C:/Users/Stefan/Downloads/osfstorage-archive/Experiment 08") #set to location of the data files

#------data analyses------#
#import data
#variant 1 (choose only one)
#dat = read.table(file = "exp08_data_rating.txt", header = TRUE)
#dat$subj = as.factor(dat$subj)

#variant 2 (choose only one)
load("exp08_prep_rating.RData")

#variant 3 (choose only one)
#dat = read.table(file = "https://osf.io/vkbcn/download", header = TRUE)
#dat$subj = as.factor(dat$subj)

#data structure
str(dat)
#- subj = participant ID
#- trial = trial number
#- rating = rating score (1: little visual similarity; 5: strong visual similarity)
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

#---------------------------------semantic_form vs.unrelated_noform

#------------------------participant analysis
res = t.test(agg.subj.wide$rat.semantic_form, agg.subj.wide$rat.unrelated_noform, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide$rat.semantic_form, agg.subj.wide$rat.unrelated_noform, method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide$rat.semantic_form, agg.item.wide$rat.unrelated_noform, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide$rat.semantic_form, agg.item.wide$rat.unrelated_noform, method = "paired")

#-------------------------------semantic_noform vs.unrelated_noform

#------------------------participant analysis
res = t.test(agg.subj.wide$rat.semantic_noform, agg.subj.wide$rat.unrelated_noform, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide$rat.semantic_noform, agg.subj.wide$rat.unrelated_noform, method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide$rat.semantic_noform, agg.item.wide$rat.unrelated_noform, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide$rat.semantic_noform, agg.item.wide$rat.unrelated_noform, method = "paired")

#----------------------------------semantic_form vs.semantic_noform

#------------------------participant analysis
res = t.test(agg.subj.wide$rat.semantic_form, agg.subj.wide$rat.semantic_noform, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.subj.wide$rat.semantic_form, agg.subj.wide$rat.semantic_noform, method = "paired")

#-------------------------------item analysis
res = t.test(agg.item.wide$rat.semantic_form, agg.item.wide$rat.semantic_noform, paired=TRUE)
SEM = res$estimate[[1]]/res$statistic[[1]]
print(res)
print(SEM, digits=4)
cohensD(agg.item.wide$rat.semantic_form, agg.item.wide$rat.semantic_noform, method = "paired")

###################################
##################################################
#preparation for correlation of RT and rating differences

#compute rating differences
agg.subj.wide$diff_rat.sem_unr.form=agg.subj.wide$rat.semantic_form-agg.subj.wide$rat.unrelated_noform
agg.subj.wide$diff_rat.sem_unr.noform=agg.subj.wide$rat.semantic_noform-agg.subj.wide$rat.unrelated_noform

agg.item.wide$diff_rat.sem_unr.form=agg.item.wide$rat.semantic_form-agg.item.wide$rat.unrelated_noform
agg.item.wide$diff_rat.sem_unr.noform=agg.item.wide$rat.semantic_noform-agg.item.wide$rat.unrelated_noform


#import RT data
dat.subj = read.table(file = "exp08_RT.subj.txt", header = TRUE, sep = ",", check.names = FALSE)
dat.item = read.table(file = "exp08_RT.item.txt", header = TRUE, sep = ",", check.names = FALSE)

#combine data sets
agg.subj.wide.all = merge(agg.subj.wide, dat.subj, by="subj") 
agg.item.wide.all = merge(agg.item.wide, dat.item, by="targ_ID") 

#-------------------------------------------------------------correlation----------------
#RT ~ rating

#---------------------------------semantic_form vs.unrelated_noform

#------------------------participant analysis
cor.test(agg.subj.wide.all$diff_RT.sem_unr.form, agg.subj.wide.all$diff_rat.sem_unr.form)

#plot
ggscatter(agg.subj.wide.all, x = "diff_rat.sem_unr.form", y = "diff_RT.sem_unr.form", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean Rating_diff", ylab = "Mean RT_diff")

#-------------------------------item analysis
cor.test(agg.item.wide.all$diff_RT.sem_unr.form, agg.item.wide.all$diff_rat.sem_unr.form)

#plot
ggscatter(agg.item.wide.all, x = "diff_rat.sem_unr.form", y = "diff_RT.sem_unr.form", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean Rating_diff", ylab = "Mean RT_diff")

#-------------------------------semantic_noform vs.unrelated_noform

#------------------------participant analysis
cor.test(agg.subj.wide.all$diff_RT.sem_unr.noform, agg.subj.wide.all$diff_rat.sem_unr.noform)

#plot
ggscatter(agg.subj.wide.all, x = "diff_rat.sem_unr.noform", y = "diff_RT.sem_unr.noform", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean Rating_diff", ylab = "Mean RT_diff")

#-------------------------------item analysis
cor.test(agg.item.wide.all$diff_RT.sem_unr.noform, agg.item.wide.all$diff_rat.sem_unr.noform)

#plot
ggscatter(agg.item.wide.all, x = "diff_rat.sem_unr.noform", y = "diff_RT.sem_unr.noform", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean Rating_diff", ylab = "Mean RT_diff")

###################################
#save workspace
save.image(file="exp08_anova_rating.RData")
