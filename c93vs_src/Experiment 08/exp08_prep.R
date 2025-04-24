#------------------------------------------------------------------------------#
#This file contains R-code for data preparation of Experiment 8 reported in:   #
# Wöhner, S. (2018).                                                           #
#    #Natürliche Geräusche und Bilder in Benennungsaufgaben -                  #
#     Semantische Kontexteffekte innerhalb und zwischen  Stimulusmodalitäten#  #
#                                                                              #
#(c) Stefan Wöhner (stefan.woehner@uni-leipzig.de)                             #
#------------------------------------------------------------------------------

#------Setup-------#
rm(list = ls())
setwd("C:/Users/Stefan/Downloads/osfstorage-archive/Experiment 08") #set to location of the data files

#packages (install before)
library(plyr)

#------data preparation------#
#import data
#variant 1 (choose only one)
rawdat = read.table(file = "exp08_rawdata.txt", header = TRUE)
#variant 2 (choose only one)
#rawdat = read.table(file = "https://osf.io/78feg/download", header = TRUE)

#data structure
str(rawdat)
#- subj = participant ID
#- trial = trial number
#- RT = naming latency
#- x1 = NESU specific output
#- ERR.code = error code
#- eat = name of a NESU specific file, specifies events within a trial  
#- targ = file name of a target picture
#- dist = file name of a distractor picture
#- cond = distractor condition (1: semantic_form; 2: unrelated_noform; 3: semantic_noform; 4: filler)
#- pic.var = picture variant (1: variant A; 2: variant B)
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
#- ver = naming task list version

#exclude familiarization, warm-up and catch/filler trials
rawdat <- rawdat[rawdat$eat == "sonatst" & rawdat$cond!=4 & rawdat$trial>138, ]
rawdat <- rawdat[rawdat$targ_ID < 89, ]

#delete empty lines
rawdat <- rawdat[!(is.na(rawdat$subj)),]

#remove not required variables
dropvars <- c("x1", "eat", "targ", "dist", "targ_pos", "dist_pos","subset", "sem.kat", "phon.ons", 
              "LatSq1", "LatSq2", "LatSq3", "LatSq4", "pl", "occ", "ver", "cross.coding1", "cross.coding2",
              "cross.coding3", "cross.coding4", "cross.coding5", "ver.rat", "pic.var")

rawdat <- rawdat[,!(names(rawdat) %in% dropvars)]

#------specify factors------#
rawdat$subj = as.factor(rawdat$subj)
rawdat$ERR.code = factor(rawdat$ERR.code, levels=c(0,1,2,3,4,5),
                         labels=c("correct", "none/false","disfluency", "smack", "technical", "distractor named"))
rawdat$cond = factor(rawdat$cond, levels=c(1,2,3),labels=c("semantic_form", "unrelated_noform", "semantic_noform"))
#variant 1 (choose only one)
targlab <- read.table(file="targlab.txt", stringsAsFactors=F, header=F, col.names=c("id", "label"))
#variant 2 (choose only one)
#targlab <- read.table(file="https://osf.io/xqn6s/download", stringsAsFactors=F, header=F, col.names=c("id", "label"))
rawdat$targ_ID = factor(rawdat$targ_ID, levels=targlab$id, labels=targlab$label)
#variant 1 (choose only one)
distlab <- read.table(file="distlab.txt", stringsAsFactors=F, header=F, col.names=c("id", "label"))
#variant 2 (choose only one)
#distlab <- read.table(file="https://osf.io/9edu6/download", stringsAsFactors=F, header=F, col.names=c("id", "label"))
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
#M and SD per subject and item depending on distractor condition (independent variables)
subj.M.SD = ddply(rawdat,.(subj,cond), summarize, 
                  subj.M=mean(RT, na.rm=T), subj.SD=sd(RT, na.rm=T))
tar.M.SD = ddply(rawdat,.(targ_ID,cond), summarize,
                 tar.M=mean(RT, na.rm=T), tar.SD=sd(RT, na.rm=T))
rawdat = merge(rawdat, subj.M.SD, by=c("subj", "cond"))
rawdat = merge(rawdat, tar.M.SD, by=c("targ_ID", "cond"))

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
rawdat$exp=8

#---Output------#
#remove not required variables 
dropvars = c("subj.M","subj.SD","tar.M","tar.SD","subj.min","subj.max",
             "tar.min","tar.max")

dat = rawdat[,!(names(rawdat) %in% dropvars)]
dat = droplevels(dat)

#save workspace
save(list=c("dat", "rawdat"), file="exp08_prep.Rdata")

#save data
write.table(dat, "exp08_data.txt")