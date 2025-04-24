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
#- x1 = NESU specific output, rating score (1: little visual similarity; 5: strong visual similarity)
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
rawdat <- rawdat[rawdat$eat == "sonarat" & rawdat$cond!=4, ]
rawdat <- rawdat[rawdat$targ_ID < 89, ]

#delete empty lines
rawdat <- rawdat[!(is.na(rawdat$subj)),]

#rename x1 to rating
names(rawdat)[names(rawdat) == 'x1'] = 'rating'
rawdat = droplevels(rawdat)

#remove not required variables
dropvars <- c("eat", "RT", "ERR.code", "targ_pos","subset", "sem.kat", "phon.ons", 
              "LatSq1", "LatSq2", "LatSq3", "LatSq4", "blk","pl", "occ", "ver", "ver.rat", "cross.coding1", "cross.coding2",
              "cross.coding3", "cross.coding4", "cross.coding5", "pic.var")

rawdat <- rawdat[,!(names(rawdat) %in% dropvars)]

#------specify factors------#
rawdat$subj = as.factor(rawdat$subj)
rawdat$cond = factor(rawdat$cond, levels=c(1,2,3),labels=c("semantic_form", "unrelated_noform", "semantic_noform"))
rawdat$rating = as.numeric(rawdat$rating)
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

#specify experiment number
rawdat$exp=8

###################################
##################################################
#duplicate ratings per item in semantic conditions for item analysis

#separate data set
rawdat.unr=rawdat[rawdat$cond=="unrelated_noform",]
rawdat.sem1=rawdat[rawdat$cond=="semantic_form" |  rawdat$cond=="semantic_noform",]
rawdat.sem2=rawdat.sem1

#create targ_ID backup
rawdat.sem2$targ_ID.bck=rawdat.sem2$targ_ID

#rename targ_ID
rawdat.sem2$targ_ID=rawdat.sem2$dist_ID
#rename dist_ID
rawdat.sem2$dist_ID=rawdat.sem2$targ_ID.bck

#remove not required variables
dropvars <- c("targ_ID.bck")
rawdat.sem2 <- rawdat.sem2[,!(names(rawdat.sem2) %in% dropvars)]

#combine data sets
rawdat.new=rbind(rawdat.unr,rawdat.sem1,rawdat.sem2)

#remove not required variables
dropvars <- c("targ", "dist")

rawdat.new <- rawdat.new[,!(names(rawdat.new) %in% dropvars)]

#---Output------#

dat=rawdat.new

#save workspace
save(list=c("dat", "rawdat.new"), file="exp08_prep_rating.Rdata")

#save data
write.table(rawdat.new, "exp08_data_rating.txt")
