#------------------------------------------------------------------------------#
#This file contains R-code for data preparation of Experiment 3 reported in:   #
# Wöhner, S. (2018).                                                           #
#    #Natürliche Geräusche und Bilder in Benennungsaufgaben -                  #
#     Semantische Kontexteffekte innerhalb und zwischen  Stimulusmodalitäten#  #
#                                                                              #
#(c) Stefan Wöhner (stefan.woehner@uni-leipzig.de)                             #
#------------------------------------------------------------------------------

#------Setup-------#
rm(list = ls())
setwd("C:/Users/Stefan/Downloads/osfstorage-archive/Experiment 03") #set to location of the data files

#packages (install before)
library(plyr)

#------data preparation------#
#import data
#variant 1 (choose only one)
rawdat = read.table(file = "exp03_rawdata.txt", header = TRUE)
#variant 2 (choose only one)
#rawdat = read.table(file = "https://osf.io/7adxt/download", header = TRUE)

#data structure
str(rawdat)
#- subj = participant ID
#- trial = trial number
#- RT = naming latency
#- BPR_Resp = NESU specific output ("/" refers to missing or verbal responses; ">", "<" refers to manual responses)
#- ERR.code = error types
#- eat = name of a NESU specific file, specifies events within a trial  
#- targ = file name of a target sound
#- dist = file name of a distractor picture
#- BPR_pic = file name of task 1 stimuli (specifies letter version and angle of rotation)
#- BPR_corrResp = expected/correct response of task 1 stimuli
#- BPR_pos = position of task 1 stimuli (above/below the pictures)
#- BPR_let = letter (B; F; G; K; L; P; Q; Z)
#- BPR_ver = letter version (normal; mirrored)
#- BPR_rot = angle of rotation of task 1 stimuli (150°; 210°)
#- task.soa = task stimulus-onset-asynchrony (0 ms; 1,500 ms)
#- cond = distractor condition (1: congruent; 2: semantic; 3: unrelated; 4: filler)
#- targ_ID = target sound ID
#- subset = item subset
#- dist_ID = distractor picture ID
#- sem.pair = index of semantically related item pairs (1-16; e.g., "Hahn-Ente")
#- blk = experimental block (each task.soa was presented equally often in each block)
#- pl = parallel list
#- occ = occurence of a target sound
#- sem.kat = semantic category of a target sound (e.g., animal, vehicle, ...)
#- phon.ons = phonological onset/initial phonologocal units of a sound name
#- LatSq(1-4) = latin square
#- cross.coding(1-5) = index of sounds and corresponding pictures to avoid occurences on consecutive trials
#- ver = list version

#exclude familiarization, warm-up and catch/filler trials
rawdat <- rawdat[rawdat$eat == "sonaPRP" & rawdat$cond!=4,]
rawdat <- rawdat[rawdat$targ_ID < 89,]

#remove not required variables
dropvars <- c("eat", "targ", "dist", "subset",
              "sem.pair", "sem.kat", "phon.ons", "LatSq1", "LatSq2", "LatSq3", "LatSq4", 
              "cross.coding1", "cross.coding2","cross.coding3", "cross.coding4", "cross.coding5", 
              "pl", "occ", "ver")

rawdat <- rawdat[,!(names(rawdat) %in% dropvars)]

#recode errors
rawdat$ERR.code.old=rawdat$ERR.code
rawdat$ERR.code[rawdat$ERR.code == "+"] = 0
rawdat$ERR.code[rawdat$ERR.code == "-"] = 1
rawdat$ERR.code = droplevels(rawdat$ERR.code)
rawdat$ERR.code = as.character(rawdat$ERR.code)

#match trials of tasks 1 & 2, handle empty lines

#compute rows 1:n
rawdat$row <- 1:nrow(rawdat)

#split rawdata
#task 1
rawdat.mRT = rawdat[(rawdat$BPR_Resp == "<") | (rawdat$BPR_Resp == ">") | (rawdat$BPR_Resp == "-"),]
#task 2
rawdat.vRT = rawdat[(rawdat$BPR_Resp == "/") | (rawdat$BPR_Resp == "-"),]
#merge data depending on trial and participant ID
rawdat.all = merge(rawdat.mRT, rawdat.vRT, by = c("trial", "subj"), all = T)

#compute row differences
#negative differences reflect correct task order (task 1, task 2)
#positive differences reflect wrong task order (task 2, task 1)
#differences equal to zero reflect identical pairings (task 1, task 1; task 2, task 2)
rawdat.all$row_diff = rawdat.all$row.x - rawdat.all$row.y

#check duplicate trials per participant (prae)
xtabs(~subj+trial,data=rawdat.all)
table(rawdat.all$subj)

#delete duplicates
rawdat.all$i = 0
rawdat.all$i[duplicated(rawdat.all[,c('subj', 'trial')]) | duplicated(rawdat.all[,c('subj', 'trial')], fromLast =  TRUE)] = 1
rawdat.all = rawdat.all[((rawdat.all$row_diff < 0) & (rawdat.all$i == 1)) | (rawdat.all$i == 0),]

#check duplicate trials per participant (post)
xtabs(~subj+trial,data=rawdat.all)
table(rawdat.all$subj)

#save rawdata
rawdat.old = rawdat
rawdat = rawdat.all

#delete empty lines
rawdat <- rawdat[!(is.na(rawdat$subj)),]

#------error coding------#
rawdat$ERR.code.x[is.na(rawdat$ERR.code.x)] = 1
rawdat$ERR.code.y[is.na(rawdat$ERR.code.y)] = 1
rawdat$ERR.code.x[rawdat$row_diff>0] = 6
rawdat$ERR.code.y[rawdat$row_diff>0] = 6
rawdat$ERR.code.x[rawdat$row_diff==0] = 1
rawdat$ERR.code.y[rawdat$row_diff==0] = 1

#task 1 errors
rawdat$ERR.task1 = 0
rawdat$ERR.task1[rawdat$ERR.code.x!=0 & rawdat$ERR.code.x!=4] = 1

#task 2 errors
#including participant and technical errors
rawdat$ERR.task2.old = 0
rawdat$ERR.task2.old[rawdat$ERR.code.y!=0] = 1

#task 2 errors
#excluding technical (non-participant) errors
rawdat$ERR.task2 = 0
rawdat$ERR.task2[rawdat$ERR.code.y!=0 & rawdat$ERR.code.y!=4] = 1

#remove not required variables 
dropvars = c("BPR_Resp.x","BPR_corrResp.x", "task.soa.x","cond.x","targ_ID.x","dist_ID.x",
             "blk.x", "row.x", "ERR.code.old.x", "BPR_Resp.y",
             "BPR_corrResp.y", "row.y", "ERR.code.old.y", "row_diff", "i", "j", "BPR_pic.x", 
             "BPR_pic.y", "BPR_pos.x", "BPR_pos.y", "BPR_let.x", "BPR_ver.x", "BPR_rot.x", "BPR_rot.y")

rawdat = rawdat[,!(names(rawdat) %in% dropvars)]
rawdat = droplevels(rawdat)

#rename variables
names(rawdat) <- c("trial", "subj", "RT.task1", "ERR.code.task1", "RT.task2", "ERR.code.task2", "task1.let", "task1.let_ver", 
                   "task.soa", "cond", "targ_ID", "dist_ID", "blk", "ERR.task1", "ERR.task2.old", "ERR.task2")

#------specify factors------#
rawdat$subj = as.factor(rawdat$subj)
rawdat$ERR.code.task1 = factor(rawdat$ERR.code.task1, levels=c(0,1,6),
                               labels=c("correct", "none/false", "wrong task order"))
rawdat$ERR.code.task2 = factor(rawdat$ERR.code.task2, levels=c(0,1,2,3,4,5,6),
                               labels=c("correct", "none/false","disfluency", "smack", "technical", "distractor named", 
                                        "wrong task order"))
rawdat$cond = factor(rawdat$cond, levels=c(1,2,3),labels=c("congruent", "semantic", "unrelated"))
rawdat$task.soa = as.factor(rawdat$task.soa)
#variant 1 (choose only one)
targlab <- read.table(file="targlab.txt", stringsAsFactors=F, header=F, col.names=c("id", "label"))
#variant 2 (choose only one)
#targlab <- read.table(file="https://osf.io/457j8/download", stringsAsFactors=F, header=F, col.names=c("id", "label"))
rawdat$targ_ID = factor(rawdat$targ_ID, levels=targlab$id, labels=targlab$label)
#variant 1 (choose only one)
distlab <- read.table(file="distlab.txt", stringsAsFactors=F, header=F, col.names=c("id", "label"))
#variant 2 (choose only one)
#distlab <- read.table(file="https://osf.io/ufg3d/download", stringsAsFactors=F, header=F, col.names=c("id", "label"))
rawdat$dist_ID = factor(rawdat$dist_ID, levels=distlab$id, labels=distlab$label)
rawdat$blk = as.factor(rawdat$blk)
rawdat$task1.let = as.factor(rawdat$task1.let)
rawdat$task1.let_ver = factor(rawdat$task1.let_ver, levels=c("norm","gesp"),labels=c("normal", "mirrored"))

#exclude erroneous responses and RTs >=3,000/4,500 ms and RTs<=100/300 ms from latency analyses
#note: response window = 3,000 ms (task 1 at task.soa = 0 ms; task 2); 4,500 ms (task 1 at task.soa = 1,500 ms);
#      latencies of less than 100 ms do not reflect regular manual responses (task 1)
#      latencies of less than 300 ms do not reflect regular speech production processes (task 2)
rawdat$RT.task1.old = rawdat$RT.task1
rawdat$RT.task2.old = rawdat$RT.task2
rawdat$RT.task1[(rawdat$ERR.code.task1!="correct")] = NA
rawdat$RT.task2[(rawdat$ERR.code.task2!="correct")] = NA
rawdat$RT.task1[rawdat$task.SOA==0 & rawdat$RT.task1>=3000 | rawdat$RT.task1<=100] = NA
rawdat$RT.task1[rawdat$task.SOA==1500 & rawdat$RT.task1>=4500 | rawdat$RT.task1<=100] = NA
rawdat$RT.task2[rawdat$RT.task2>=3000 | rawdat$RT.task2<=300] = NA

#------outlier coding------#
#M and SD per subject and item depending on distractor condition and task.soa (independent variables)
subj.M.SD = ddply(rawdat,.(subj,cond,task.soa), summarize, 
                  subj.M=mean(RT.task2, na.rm=T), subj.SD=sd(RT.task2, na.rm=T))
tar.M.SD = ddply(rawdat,.(targ_ID,cond,task.soa), summarize,
                 tar.M=mean(RT.task2, na.rm=T), tar.SD=sd(RT.task2, na.rm=T))
rawdat = merge(rawdat, subj.M.SD, by=c("subj", "cond", "task.soa"))
rawdat = merge(rawdat, tar.M.SD, by=c("targ_ID", "cond", "task.soa"))

rawdat$subj.min = (rawdat$subj.M - 2*(rawdat$subj.SD))
rawdat$subj.max = (rawdat$subj.M + 2*(rawdat$subj.SD))
rawdat$tar.min = (rawdat$tar.M - 2*(rawdat$tar.SD))
rawdat$tar.max = (rawdat$tar.M + 2*(rawdat$tar.SD))

#define outlier
rawdat$out = 0
rawdat$out[(rawdat$RT.task2 < rawdat$subj.min) & (rawdat$RT.task2 < rawdat$tar.min)] = 1
rawdat$out[(rawdat$RT.task2 > rawdat$subj.max) & (rawdat$RT.task2 > rawdat$tar.max)] = 1

#remove ouliers from latency analyses
rawdat$RT.task2[rawdat$out == 1] = NA

#specify experiment number
rawdat$exp=3

#---Output------#
#remove not required variables 
dropvars = c("subj.M","subj.SD","tar.M","tar.SD","subj.min","subj.max",
             "tar.min","tar.max")

dat = rawdat[,!(names(rawdat) %in% dropvars)]
dat = droplevels(dat)

#save workspace
save(list=c("dat", "rawdat"), file="exp03_prep.Rdata")

#save data
write.table(dat, "exp03_data.txt")