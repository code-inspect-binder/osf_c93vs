#------------------------------------------------------------------------------#
#This file contains R-code for data preparation of Experiment 4 reported in:   #
# Wöhner, S. (2018).                                                           #
#    #Natürliche Geräusche und Bilder in Benennungsaufgaben -                  #
#     Semantische Kontexteffekte innerhalb und zwischen  Stimulusmodalitäten#  #
#                                                                              #
#(c) Stefan Wöhner (stefan.woehner@uni-leipzig.de)                             #
#------------------------------------------------------------------------------

#------Setup-------#
rm(list = ls())
setwd("C:/Users/Stefan/Downloads/osfstorage-archive/Experiment 04") #set to location of the data files

#packages (install before)
library(plyr)

#------data preparation------#
#import data
#variant 1 (choose only one)
rawdat = read.table(file = "exp04_rawdata.txt", header = TRUE)
#variant 2 (choose only one)
#rawdat = read.table(file = "https://osf.io/ezavd/download", header = TRUE)

#data structure
str(rawdat)
#- subj = participant ID
#- trial = trial number
#- RT = naming latency
#- x1 = NESU specific output (e.g., refers to manual responses in catch trials)
#- ERR.code = error code
#- eat = name of a NESU specific file, specifies events within a trial  
#- targ = file name of a target picture
#- dist = file name of a distractor picture
#- cat.dir = file name of a masked arrow
#- cat.corr = direction of a masked arrow (expected/correct response)
#- jitter = onset of a masked arrow
#- soa = stimulus-onset-asynchrony (-200 ms; 0 ms)
#- cond = distractor condition (1: congruent; 2: semantic; 3: unrelated; 4: catch)
#- targ_pos = position of a target picture
#- targ_ID = target picture  ID
#- subset = item subset
#- dist_ID = distractor picture ID
#- blk = experimental block (SOA was blocked and counterbalanced)
#- pl = parallel list
#- occ = occurence of a target picture
#- sem.kat = semantic category of a target picture (e.g., animal, vehicle, ...)
#- phon.ons = phonological onset/initial phonologocal units of a picture name
#- LatSq(1-4) = latin square
#- cross.coding(1-5) = index of target pictures and corresponding distractor pictures to avoid occurences on consecutive trials
#- ver = list version

#exclude familiarization, warm-up and catch/filler trials
rawdat <- rawdat[rawdat$eat == "sonatst", ]
rawdat <- rawdat[rawdat$targ_ID < 89, ]

#delete empty lines
rawdat <- rawdat[!(is.na(rawdat$subj)),]

#remove not required variables
dropvars <- c("x1", "eat", "targ", "dist", "cat.dir", "cat.corr", "targ_pos","subset", "jitter", "sem.kat", "phon.ons", 
              "LatSq1", "LatSq2", "LatSq3", "LatSq4", "pl", "occ", "ver", "cross.coding1", "cross.coding2",
              "cross.coding3", "cross.coding4", "cross.coding5")

rawdat <- rawdat[,!(names(rawdat) %in% dropvars)]

#------specify factors------#
rawdat$subj = as.factor(rawdat$subj)
rawdat$ERR.code = factor(rawdat$ERR.code, levels=c(0,1,2,3,4,5),
                         labels=c("correct", "none/false","disfluency", "smack", "technical", "distractor named"))
rawdat$cond = factor(rawdat$cond, levels=c(1,2,3),labels=c("congruent", "semantic", "unrelated"))
rawdat$soa = as.factor(rawdat$soa)
#variant 1 (choose only one)
targlab <- read.table(file="targlab.txt", stringsAsFactors=F, header=F, col.names=c("id", "label"))
#variant 2 (choose only one)
#targlab <- read.table(file="https://osf.io/3emcb/download", stringsAsFactors=F, header=F, col.names=c("id", "label"))
rawdat$targ_ID = factor(rawdat$targ_ID, levels=targlab$id, labels=targlab$label)
#variant 1 (choose only one)
distlab <- read.table(file="distlab.txt", stringsAsFactors=F, header=F, col.names=c("id", "label"))
#variant 2 (choose only one)
#distlab <- read.table(file="https://osf.io/tde3h/download", stringsAsFactors=F, header=F, col.names=c("id", "label"))
rawdat$dist_ID = factor(rawdat$dist_ID, levels=distlab$id, labels=distlab$label)
rawdat$blk = as.factor(rawdat$blk)

#------error coding------#
#including participant and technical errors
rawdat$ERR.old = 0
rawdat$ERR.old[rawdat$ERR.code!="correct"] = 1

#excluding technical (non-participant) errors
rawdat$ERR = 0
rawdat$ERR[rawdat$ERR.code!="correct" & rawdat$ERR.code!="technical"] = 1

#exclude erroneous responses and RTs >=3,000 ms and RTs<=300 ms from latency analyses
#note: response window = 3,000 ms; latencies of less than 300 ms do not reflect regular speech production processes
rawdat$RT.old = rawdat$RT
rawdat$RT[rawdat$ERR.code!="correct"] = NA
rawdat$RT[rawdat$RT>=3000 | rawdat$RT<=300] = NA

#------outlier coding------#
#M and SD per subject and item depending on distractor condition and soa (independent variables)
subj.M.SD = ddply(rawdat,.(subj,cond,soa), summarize, 
                  subj.M=mean(RT, na.rm=T), subj.SD=sd(RT, na.rm=T))
tar.M.SD = ddply(rawdat,.(targ_ID,cond,soa), summarize,
                 tar.M=mean(RT, na.rm=T), tar.SD=sd(RT, na.rm=T))
rawdat = merge(rawdat, subj.M.SD, by=c("subj", "cond", "soa"))
rawdat = merge(rawdat, tar.M.SD, by=c("targ_ID", "cond", "soa"))

rawdat$subj.min = (rawdat$subj.M - 2*(rawdat$subj.SD))
rawdat$subj.max = (rawdat$subj.M + 2*(rawdat$subj.SD))
rawdat$tar.min = (rawdat$tar.M - 2*(rawdat$tar.SD))
rawdat$tar.max = (rawdat$tar.M + 2*(rawdat$tar.SD))

#define outlier
rawdat$out = 0
rawdat$out[(rawdat$RT < rawdat$subj.min) & (rawdat$RT < rawdat$tar.min)] = 1
rawdat$out[(rawdat$RT > rawdat$subj.max) & (rawdat$RT > rawdat$tar.max)] = 1

#remove ouliers from latency analyses
rawdat$RT[rawdat$out == 1] = NA

#specify experiment number
rawdat$exp=4

#---Output------#
#remove not required variables 
dropvars = c("subj.M","subj.SD","tar.M","tar.SD","subj.min","subj.max",
             "tar.min","tar.max")

dat = rawdat[,!(names(rawdat) %in% dropvars)]
dat = droplevels(dat)

#save workspace
save(list=c("dat", "rawdat"), file="exp04_prep.Rdata")

#save data
write.table(dat, "exp04_data.txt")