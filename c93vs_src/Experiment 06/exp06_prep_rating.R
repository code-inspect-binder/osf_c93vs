#------------------------------------------------------------------------------#
#This file contains R-code for data preparation of Experiment 6 reported in:   #
# Wöhner, S. (2018).                                                           #
#    #Natürliche Geräusche und Bilder in Benennungsaufgaben -                  #
#     Semantische Kontexteffekte innerhalb und zwischen  Stimulusmodalitäten#  #
#                                                                              #
#(c) Stefan Wöhner (stefan.woehner@uni-leipzig.de)                             #
#------------------------------------------------------------------------------

#------Setup-------#
rm(list = ls())
setwd("C:/Users/Stefan/Downloads/osfstorage-archive/Experiment 06") #set to location of the data files

#packages (install before)
library(plyr)

#------data preparation------#
#import data
#variant 1 (choose only one)
rawdat = read.table(file = "exp06_rawdata.txt", header = TRUE)
#variant 2 (choose only one)
#rawdat = read.table(file = "https://osf.io/ewxu8/download", header = TRUE)

#data structure
str(rawdat)
#- subj = participant ID
#- trial = trial number
#- RT = naming latency
#- x1 = NESU specific output, rating score (1: target picture poorly recognizable; 5: target picture well recognizable)
#- ERR.code = error code
#- eat = name of a NESU specific file, specifies events within a trial  
#- targ = file name of a target picture
#- dist = file name of a distractor picture
#- soa = stimulus-onset-asynchrony (-200 ms; 0 ms)
#- cond = distractor condition (1: congruent; 2: semantic; 3: unrelated; 4: catch)
#- targ_pos = position of a target picture
#- dist_pos = position of a distractor picture
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
rawdat <- rawdat[rawdat$eat == "sonarat" & rawdat$cond!=4, ]
rawdat <- rawdat[rawdat$targ_ID < 89, ]

#delete empty lines
rawdat <- rawdat[!(is.na(rawdat$subj)),]

#rename x1 to rating
names(rawdat)[names(rawdat) == 'x1'] = 'rating'
rawdat = droplevels(rawdat)

#remove not required variables
dropvars <- c("eat", "RT", "ERR.code", "targ", "dist", "targ_pos", "dist_pos","subset", "sem.kat", "phon.ons", 
              "LatSq1", "LatSq2", "LatSq3", "LatSq4", "soa", "blk","pl", "occ", "ver", "cross.coding1", "cross.coding2",
              "cross.coding3", "cross.coding4", "cross.coding5")

rawdat <- rawdat[,!(names(rawdat) %in% dropvars)]

#------specify factors------#
rawdat$subj = as.factor(rawdat$subj)
rawdat$cond = factor(rawdat$cond, levels=c(1,2,3),labels=c("congruent", "semantic", "unrelated"))
rawdat$rating = as.numeric(rawdat$rating)
#variant 1 (choose only one)
targlab <- read.table(file="targlab.txt", stringsAsFactors=F, header=F, col.names=c("id", "label"))
#variant 2 (choose only one)
#targlab <- read.table(file="https://osf.io/7fzgt/download", stringsAsFactors=F, header=F, col.names=c("id", "label"))
rawdat$targ_ID = factor(rawdat$targ_ID, levels=targlab$id, labels=targlab$label)
#variant 1 (choose only one)
distlab <- read.table(file="distlab.txt", stringsAsFactors=F, header=F, col.names=c("id", "label"))
#variant 2 (choose only one)
#distlab <- read.table(file="https://osf.io/qv4tc/download", stringsAsFactors=F, header=F, col.names=c("id", "label"))
rawdat$dist_ID = factor(rawdat$dist_ID, levels=distlab$id, labels=distlab$label)

#specify experiment number
rawdat$exp=6

#---Output------#

dat=rawdat

#save workspace
save(list=c("dat","rawdat"), file="exp06_prep_rating.Rdata")

#save data
write.table(rawdat, "exp06_data_rating.txt")
