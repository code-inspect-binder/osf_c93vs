#------------------------------------------------------------------------------#
#This file contains R-code for data preparation of Experiment 9 reported in:   #
# Wöhner, S. (2018).                                                           #
#    #Natürliche Geräusche und Bilder in Benennungsaufgaben -                  #
#     Semantische Kontexteffekte innerhalb und zwischen  Stimulusmodalitäten#  #
#                                                                              #
#(c) Stefan Wöhner (stefan.woehner@uni-leipzig.de)                             #
#------------------------------------------------------------------------------

#------Setup-------#
rm(list = ls())
setwd("C:/Users/Stefan/Downloads/osfstorage-archive/Experiment 09") #set to location of the data files

#packages (install before)
library(plyr)

#------data preparation------#
#import data
#variant 1 (choose only one)
rawdat = read.table(file = "exp09_rawdata.txt", header = TRUE)
#variant 2 (choose only one)
#rawdat = read.table(file = "https://osf.io/8xnfk/download", header = TRUE)

#data structure
str(rawdat)
#- subj = participant ID
#- trial = trial number
#- RT = naming latency
#- x1 = NESU specific output (e.g., refers to manual responses in catch trials)
#- ERR.code = error code
#- eat = name of a NESU specific file, specifies events within a trial  
#- targ = file name of a target picture
#- dist = file name of a distractor word
#- x2 = specifies orientation of masked arrows in catch trials (1: left; 2: right) or file name of target pictures superimposed by visual distractor words
#- soa = stimulus-onset-asynchrony (-200 ms; 0 ms; 200 ms)
#- cond = distractor condition (1: congruent; 2: unrelated; 3: catch)
#- targ_ID = target picture ID
#- subset = item subset
#- dist_ID = distractor word ID
#- cross.coding(1-3) = index of pictures and corresponding words to avoid occurences on consecutive trials
#- targ_pho = phonological onset/ initial phonological units of a picture name
#- dist_pho = phonological onset/ initial phonological units of a word name
#- targ_kat = superordinate category of a target picture (e.g., bird, mammal, vehicle, ...)
#- dist_kat = superordinate category of a distractor word (e.g., bird, mammal, vehicle, ...)
#- targ_kat_pho1 = phonological onset/ initial phonological units of a target category
#- dist_kat_pho1 = phonological onset/ initial phonological units of a distractor category
#- targ_kat_pho2 = index of syllable overlap between target categories (i.e., Fahrzeug [vehicle] & Werkzeug [tool])
#- dist_kat_pho2 = index of syllable overlap between distractor categories (i.e., Fahrzeug [vehicle] & Werkzeug [tool])
#- pl = parallel list
#- blk = experimental block (SOA was blocked and counterbalanced)
#- occ = occurence of a target picture
#- LatSq(1-2) = latin square
#- ver = list version
#- mod = distractor modality (visual, auditory)

#exclude familiarization, warm-up and catch trials
rawdat <- rawdat[rawdat$eat != "socatnon" & rawdat$eat != "socatfm2", ]
rawdat <- rawdat[rawdat$targ_ID < 89, ]

#delete empty lines
rawdat <- rawdat[!(is.na(rawdat$subj)),]

#remove not required variables
dropvars <- c("x1", "eat", "targ", "dist", "x2", "subset", "jitter", "sem.kat", "targ_pho", "dist_pho", 
              "targ_kat", "dist_kat", "targ_kat_pho1", "targ_kat_pho2", "dist_kat_pho1", "dist_kat_pho2",
              "LatSq1", "LatSq2", "pl", "occ", "ver", "cross.coding1", "cross.coding2", "cross.coding3")

rawdat <- rawdat[,!(names(rawdat) %in% dropvars)]

#------specify factors------#
rawdat$subj = as.factor(rawdat$subj)
rawdat$ERR.code = factor(rawdat$ERR.code, levels=c(0,1,2,3,4,5,6,7),
                         labels=c("correct", "none/false","disfluency", "smack", "technical", "target basic level named",
                                  "distractor basic level named", "distractor superordinate level named"))
rawdat$cond = factor(rawdat$cond, levels=c(1,2),labels=c("congruent", "unrelated"))
rawdat$soa = as.factor(rawdat$soa)
#variant 1 (choose only one)
targlab <- read.table(file="targlab.txt", stringsAsFactors=F, header=F, col.names=c("id", "label"))
#variant 2 (choose only one)
#targlab <- read.table(file="https://osf.io/yjvcw/download", stringsAsFactors=F, header=F, col.names=c("id", "label"))
rawdat$targ_ID = factor(rawdat$targ_ID, levels=targlab$id, labels=targlab$label)
#variant 1 (choose only one)
distlab <- read.table(file="distlab.txt", stringsAsFactors=F, header=F, col.names=c("id", "label"))
#variant 2 (choose only one)
#distlab <- read.table(file="https://osf.io/5bfeu/download", stringsAsFactors=F, header=F, col.names=c("id", "label"))
rawdat$dist_ID = factor(rawdat$dist_ID, levels=distlab$id, labels=distlab$label)
rawdat$blk = as.factor(rawdat$blk)
rawdat$mod = as.factor(rawdat$mod)

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
#M and SD per subject and item depending on distractor condition, distractor modality and soa (independent variables)
subj.M.SD = ddply(rawdat,.(subj,cond,soa,mod), summarize, 
                  subj.M=mean(RT, na.rm=T), subj.SD=sd(RT, na.rm=T))
tar.M.SD = ddply(rawdat,.(targ_ID,cond,soa,mod), summarize,
                 tar.M=mean(RT, na.rm=T), tar.SD=sd(RT, na.rm=T))
rawdat = merge(rawdat, subj.M.SD, by=c("subj", "cond", "soa", "mod"))
rawdat = merge(rawdat, tar.M.SD, by=c("targ_ID", "cond", "soa", "mod"))

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
rawdat$exp=9

#---Output------#
#remove not required variables 
dropvars = c("subj.M","subj.SD","tar.M","tar.SD","subj.min","subj.max",
             "tar.min","tar.max")

dat = rawdat[,!(names(rawdat) %in% dropvars)]
dat = droplevels(dat)

#save workspace
save(list=c("dat", "rawdat"), file="exp09_prep.Rdata")

#save data
write.table(dat, "exp09_data.txt")