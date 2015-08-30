#Prepare patient data in target group and control group, respectively.

#Read lab, bld culture, and vital signs data
lab <- read.csv("pts_lab_new.csv", header=TRUE)
bld<- read.csv("pts_bldcultures3_new.csv", header=TRUE)
charts <- read.csv('pts_vitals1b_new.csv')

#get the icuseq info for bld culture
icuseqs <- read.csv('icuseqs_blds.csv',header = F)
#get the hospseq info for vital signs
hospseqs <- read.csv('hospseqs_vitals.csv', header = F)

bld$icuseq <- as.vector(unlist(icuseqs), mode = 'numeric')
charts$hospseq <- as.vector(unlist(hospseqs), mode = 'numeric')

bld <- bld[bld$icuseq > 0, ]
bld$id <- paste(bld$subject_id, bld$hospital_seq, bld$icuseq, sep='#%#')
lab$id <- paste(lab$subject_id, lab$hospital_seq, lab$icustay_seq, sep='#%#')
charts$id <- paste(charts$subject_id, charts$hospseq, charts$icustay_seq, sep='#%#')


bld1 <- data.frame(bld$id, bld$charttime, bld$spec_itemid)
bld1$valuenum <- rep(NA, nrow(bld1))
names(bld1) <- c('id', 'charttime', 'itemid', 'valuenum')

lab1 <- data.frame(lab$id, lab$charttime, lab$itemid, lab$valuenum)
names(lab1) <- c('id', 'charttime', 'itemid', 'valuenum')

bp.value2 <- charts[charts$itemid == 455,]
bp.value2[,c(6)] <- bp.value2[,c(7)]
bp.value2$itemid <- 4552  
# assign dbp to new put back into chart1
chart1 <- data.frame(rbind(charts, bp.value2))
# # temperature in F
# temp.C <- chart1[chart1$itemid == 678,]
names(chart1)
chart2 <- data.frame(chart1$id, chart1$charttime, chart1$itemid, chart1$value1num)
names(chart2) <- c('id', 'charttime', 'itemid', 'valuenum')

chart2.lactate <- chart2[!is.na(chart2$itemid)&(chart2$itemid == 818 | chart2$itemid == 1531), ]
chart2.lactate.id <- unique(chart2.lactate$id) #11,172
chart2.highlactate <- chart2.lactate[!is.na(chart2.lactate$itemid)&(chart2.lactate$itemid == 818 | chart2.lactate$itemid == 1531) & chart2.lactate$valuenum > 4, ]
chart2.highlactate.id <- unique(chart2.highlactate$id) #2,351

lab1.lactate <- lab1[!is.na(lab1$itemid)&(lab1$itemid == 50010), ]
lab1.lactate.id <- unique(lab1.lactate$id) #12,541
lab1.highlactate <- lab1.lactate[!is.na(lab1.lactate$itemid)&(lab1.lactate$itemid == 50010) & lab1.lactate$valuenum > 4, ]
lab1.highlactate.id <- unique(lab1.highlactate$id) #2,609

lactate.union <- union(lab1.lactate.id, chart2.lactate.id) #13,488
highlactate.union <- union(lab1.highlactate.id, chart2.highlactate.id) #2,898

#combine the lab and bld culture data
complete <- rbind(lab1, bld1) 
complete <- rbind(complete, chart2) 

library(lubridate)
complete$charttime <- ymd_hms(complete$charttime)
complete <- complete[order(complete$charttime),]

## Add indicator columns to data frame
complete$Blood_Culture <- c(rep(0,nrow(complete)))
complete$High_Lactate <- c(rep(0,nrow(complete)))
complete$Severe_Sepsis <- c(rep(0,nrow(complete)))


#find patients with high lactate value
lactates <- complete[!is.na(complete$itemid) & (complete$itemid == 50010 | complete$itemid == 818 | complete$itemid == 1531),]       # lactate lab values
length(table(lactates$id))          # total patients had lactate tested  #24,951
highlactates <- lactates[lactates$valuenum > 4,] 

hl <- highlactates$id[!duplicated(highlactates$id) & !is.na(highlactates$id)]            # Identify individuals with a high lactate
#3,043 patients

bldcultures <- complete[!is.na(complete$itemid) & (complete$itemid == 70011 | complete$itemid == 70012),] 
bc <- bldcultures$id[!duplicated(bldcultures$id) & !is.na(bldcultures$id)]     # Identify patients with a blood culture taken (12,845)

## From 'complete' data set, select only patients who have high lactate and blood culture
hl.bc <- intersect(hl, bc)  #2,183
# hl.bc <- as.list(hl.bc)

possible.sepsis <- complete[(complete$id %in% hl.bc),] 
# Identify patients in 'possible.sepsis' data set who have missing chart times for blood culture
# bc.missing <- possible.sepsis[possible.sepsis$HALFHOUR=="" & !is.na(possible.sepsis$SPEC_ITEMID),]         # Try this command if the one right below does not work
# bc.missing <- possible.sepsis[is.na(possible.sepsis$charttime) & (possible.sepsis$itemid == 70011 | possible.sepsis$itemid == 70012),]      
# bc.missing.patients <- bc.missing[!duplicated(bc.missing$id),1] #417 pts

## Create reduced subset of 'complete' that excludes patients without chart time for blood culture
# reduced.sepsis <- possible.sepsis[!(possible.sepsis$id %in% bc.missing.patients),]
reduced.sepsis <- possible.sepsis[!is.na(possible.sepsis$charttime), ]
# Count how many patients in 'reduced.sepsis' : 
length(reduced.sepsis$id[!duplicated(reduced.sepsis$id)]) # 
### Function for above code 

z.0 <- function(complete) {
  #complete <- data[data$Sid==patient,]
  ## Identify rows with blood culture taken
  for (i in 1:nrow(complete))
    if (!is.na(complete$itemid[i]) & (complete$itemid[i] == 70011 | complete$itemid[i] == 70012))
      complete$Blood_Culture[i] <- 1 
    ## Identify rows with high lactate
    for (i in 1:nrow(complete))
      if(!is.na(complete$itemid[i]) & !is.na(complete$valuenum[i]) & (complete$itemid[i] == 50010 | complete$itemid[i] == 818 | complete$itemid[i] == 1531) & complete$valuenum[i] > 4)
        complete$High_Lactate[i] <- 1
      ## Identify rows with severe sepsis
      # Empty list for chart times for blood culture
      time.bc <- c()
      # Get chart times for blood culture
      for (i in 1:nrow(complete)) 
        if (complete$Blood_Culture[i] == 1)
          time.bc <- c((complete$charttime[i]), time.bc)
      time.bc <- rev(time.bc)
      if (!is.null(time.bc)) {
      # Assign severe sepsis to rows with high lactate that occur within 24 hours of a blood culture
      for (i in 1:nrow(complete))
          if (complete$High_Lactate[i] == 1 & min(abs(difftime(time.bc, complete$charttime[i], units="hours"))) < 24)
            complete$Severe_Sepsis[i] <- 1 }
      return(complete)
}


library(plyr)
## Apply function z.0 to create data set with proper labels for Blood Culture, High Lactate, and Severe Sepsis
data1 <- ddply(reduced.sepsis, .(id), z.0) 

## Identify patients from 'data1' who have severe sepsis
severe.sepsis.observations <- data1[data1$Severe_Sepsis==1,]
severe.sepsis.pt <- severe.sepsis.observations[!duplicated(severe.sepsis.observations$id),1] 

## Count number of patients classified with severe.sepsis: 1,397
length(severe.sepsis.pt)

## Create new data set with patients who have severe sepsis
severe.sepsis <- data1[data1$id %in% severe.sepsis.pt,]
save(severe.sepsis, file = 'severe.sepsis.RData')
# load(file = 'severe.sepsis.RData')
severe.sepsis.pt <- unique(severe.sepsis$id)
severe.sepsis.ptids <- data.frame(strsplit(as.character(severe.sepsis.pt), '#%#'))
severe.sepsis.ptids2 <- data.frame(t(severe.sepsis.ptids))
colnames(severe.sepsis.ptids2) <- c("subject_id", "hospital_seq", "icustay_seq")
write.csv(severe.sepsis.ptids2, "severe.sepsis.ptids2.csv", row.names = F)

severe.sepsis.data <- reduced.sepsis[reduced.sepsis$id %in% severe.sepsis.pt, ]
severe.sepsis.lactates <- severe.sepsis.data[severe.sepsis.data$itemid == 50010 | severe.sepsis.data$itemid == 818 | severe.sepsis.data$itemid == 1531, ]
save(severe.sepsis.lactates, file = 'severe.sepsis.lactates.RData') #the lacate values of all severe sepsis patients

#===================definition 6: lactate > 2.5=======================
chart2.highlactate <- chart2.lactate[!is.na(chart2.lactate$itemid)&(chart2.lactate$itemid == 818 | chart2.lactate$itemid == 1531) & chart2.lactate$valuenum > 2.5, ]
chart2.highlactate.id <- unique(chart2.highlactate$id) #2,351

lab1.lactate <- lab1[!is.na(lab1$itemid)&(lab1$itemid == 50010), ]
lab1.lactate.id <- unique(lab1.lactate$id) #12,541
lab1.highlactate <- lab1.lactate[!is.na(lab1.lactate$itemid)&(lab1.lactate$itemid == 50010) & lab1.lactate$valuenum > 2.5, ]
lab1.highlactate.id <- unique(lab1.highlactate$id) #2,609

lactate.union <- union(lab1.lactate.id, chart2.lactate.id) #13,488
highlactate.union <- union(lab1.highlactate.id, chart2.highlactate.id) #2,898

#combine the lab and bld culture data
complete <- rbind(lab1, bld1) 
complete <- rbind(complete, chart2) 

library(lubridate)
complete$charttime <- ymd_hms(complete$charttime)
complete <- complete[order(complete$charttime),]

## Add indicator columns to data frame
complete$Blood_Culture <- c(rep(0,nrow(complete)))
complete$High_Lactate <- c(rep(0,nrow(complete)))
complete$Severe_Sepsis <- c(rep(0,nrow(complete)))


#find patients with high lactate value
lactates <- complete[!is.na(complete$itemid) & (complete$itemid == 50010 | complete$itemid == 818 | complete$itemid == 1531),]       # lactate lab values
length(table(lactates$id))          # total patients had lactate tested  #24,951
highlactates <- lactates[lactates$valuenum > 2.5,] 

hl <- highlactates$id[!duplicated(highlactates$id) & !is.na(highlactates$id)]            # Identify individuals with a high lactate
#3,043 patients

bldcultures <- complete[!is.na(complete$itemid) & (complete$itemid == 70011 | complete$itemid == 70012),] 
bc <- bldcultures$id[!duplicated(bldcultures$id) & !is.na(bldcultures$id)]     # Identify patients with a blood culture taken (12,845)

## From 'complete' data set, select only patients who have high lactate and blood culture
hl.bc <- intersect(hl, bc)  #2,183
# hl.bc <- as.list(hl.bc)

possible.sepsis <- complete[(complete$id %in% hl.bc),] 
# Identify patients in 'possible.sepsis' data set who have missing chart times for blood culture
# bc.missing <- possible.sepsis[possible.sepsis$HALFHOUR=="" & !is.na(possible.sepsis$SPEC_ITEMID),]         # Try this command if the one right below does not work
# bc.missing <- possible.sepsis[is.na(possible.sepsis$charttime) & (possible.sepsis$itemid == 70011 | possible.sepsis$itemid == 70012),]      
# bc.missing.patients <- bc.missing[!duplicated(bc.missing$id),1] #417 pts

## Create reduced subset of 'complete' that excludes patients without chart time for blood culture
# reduced.sepsis <- possible.sepsis[!(possible.sepsis$id %in% bc.missing.patients),]
reduced.sepsis <- possible.sepsis[!is.na(possible.sepsis$charttime), ]
# Count how many patients in 'reduced.sepsis' : 
length(reduced.sepsis$id[!duplicated(reduced.sepsis$id)]) # 
### Function for above code 

z.0 <- function(complete) {
  #complete <- data[data$Sid==patient,]
  ## Identify rows with blood culture taken
  for (i in 1:nrow(complete))
    if (!is.na(complete$itemid[i]) & (complete$itemid[i] == 70011 | complete$itemid[i] == 70012))
      complete$Blood_Culture[i] <- 1 
    ## Identify rows with high lactate
    for (i in 1:nrow(complete))
      if(!is.na(complete$itemid[i]) & !is.na(complete$valuenum[i]) & (complete$itemid[i] == 50010 | complete$itemid[i] == 818 | complete$itemid[i] == 1531) & complete$valuenum[i] > 2.5)
        complete$High_Lactate[i] <- 1
      ## Identify rows with severe sepsis
      # Empty list for chart times for blood culture
      time.bc <- c()
      # Get chart times for blood culture
      for (i in 1:nrow(complete)) 
        if (complete$Blood_Culture[i] == 1)
          time.bc <- c((complete$charttime[i]), time.bc)
      time.bc <- rev(time.bc)
      if (!is.null(time.bc)) {
        # Assign severe sepsis to rows with high lactate that occur within 24 hours of a blood culture
        for (i in 1:nrow(complete))
          if (complete$High_Lactate[i] == 1 & min(abs(difftime(time.bc, complete$charttime[i], units="hours"))) < 24)
            complete$Severe_Sepsis[i] <- 1 }
      return(complete)
}


library(plyr)
## Apply function z.0 to create data set with proper labels for Blood Culture, High Lactate, and Severe Sepsis
data1 <- ddply(reduced.sepsis, .(id), z.0) 

## Identify patients from 'data1' who have severe sepsis
severe.sepsis.observations <- data1[data1$Severe_Sepsis==1,]
severe.sepsis.pt <- severe.sepsis.observations[!duplicated(severe.sepsis.observations$id),1] #1,397

## Count number of patients classified with severe.sepsis: 1,397
length(severe.sepsis.pt)

## Create new data set with patients who have severe sepsis
severe.sepsis <- data1[data1$id %in% severe.sepsis.pt,]
save(severe.sepsis.df6, file = 'severe.sepsis.RData')
# load(file = 'severe.sepsis.RData')
severe.sepsis.pt <- unique(severe.sepsis$id)
severe.sepsis.ptids <- data.frame(strsplit(as.character(severe.sepsis.pt), '#%#'))
severe.sepsis.ptids2 <- data.frame(t(severe.sepsis.ptids))
colnames(severe.sepsis.ptids2) <- c("subject_id", "hospital_seq", "icustay_seq")
write.csv(severe.sepsis.ptids2, "severe.sepsis.ptids2.csv", row.names = F)



#==========================analyze the time from admission to ICU to onset of severe sepsis==================
ptsinfos2 <- read.csv('pticu_infos.csv', header = T)
ptsinfos2$id <- paste(ptsinfos2$subject_id, ptsinfos2$hospital_seq, ptsinfos2$icustay_seq, sep='#%#')
names(ptsinfos2)
icuadms <- data.frame(ptsinfos2$id, ptsinfos2$icustay_intime)
names(icuadms) <- c('id', 'intime')


# severe.sepsis.all <- severe.sepsis[order(severe.sepsis$id, severe.sepsis$charttime), ]
severe.sepsis.all <- severe.sepsis[order(severe.sepsis$id, severe.sepsis$charttime), ]
sepsis.events <- data.frame(matrix(ncol = ncol(severe.sepsis.all), nrow = 0))

z.1b <- function(dataset){
  # Identify time of event
  x <- dataset[which(dataset$Severe_Sepsis==1)[1],]    # x = time of event
  sepsis.events <- rbind(sepsis.events, x)
  return(sepsis.events)
}

severe.sepsis.eventtime <- ddply(severe.sepsis.all, .(id), z.1b)
severe.sepsis.eventtime.2 <- merge(severe.sepsis.eventtime, icuadms, by.x = "id", by.y = "id", all.x = T)
names(severe.sepsis.eventtime.2)
severe.sepsis.eventtime.2$charttime <- ymd_hms(severe.sepsis.eventtime.2$charttime)
severe.sepsis.eventtime.2$intime <- ymd_hms(severe.sepsis.eventtime.2$intime)

for (i in 1:nrow(severe.sepsis.eventtime.2))
  severe.sepsis.eventtime.2$dftime[i] <- difftime(severe.sepsis.eventtime.2$charttime[i], severe.sepsis.eventtime.2$intime[i], units="hours")


severe.sepsis2.eventtime.3 <- severe.sepsis.eventtime.2[order(severe.sepsis.eventtime.2$dftime),]
save(severe.sepsis2.eventtime.3, file = 'severe.sepsis2.eventtime.3.RData')


d <- density(severe.sepsis.eventtime.3$dftime)
plot(d, main = 'Time from ICU admission to onset of severe sepsis')
d <- density(severe.sepsis.eventtime.3$dftime[1:1500])
plot(d, main = 'Time from ICU admission to onset of severe sepsis[1:1100]')
hist(severe.sepsis.eventtime.3$dftime, main = 'Time from ICU admission to onset of severe sepsis')
hist(severe.sepsis.eventtime.3$dftime[1:1500], probability = T, main = 'Time from ICU admission to onset of severe sepsis[1:1100]', ylim=c(0,0.25))
lines(density(severe.sepsis.eventtime.3$dftime[1:1500]))  
boxplot(severe.sepsis.eventtime.3$dftime)
boxplot(severe.sepsis.eventtime.3$dftime[1:1500])
summary(severe.sepsis.eventtime.3$dftime)
summary(severe.sepsis.eventtime.3$dftime[1:1500])

d <- density(severe.sepsis2.eventtime.3$dftime)
plot(d, main = 'Time from ICU admission to onset of severe sepsis')

boxplot(severe.sepsis2.eventtime.3$dftime)
summary(severe.sepsis2.eventtime.3$dftime)
boxplot(severe.sepsis2.eventtime.3$dftime[1:2000])
summary(severe.sepsis2.eventtime.3$dftime[1:2000])
d <- density(severe.sepsis2.eventtime.3$dftime[1:2000])
plot(d, main = 'Time from ICU admission to onset of severe sepsis[1:2000]')
hist(severe.sepsis2.eventtime.3$dftime[1:2000], probability = T, main = 'Time from ICU admission to onset of severe sepsis[1:2000]', ylim=c(0,0.25))
lines(density(severe.sepsis2.eventtime.3$dftime[1:2000]))  
## For patients with Severe Sepsis indicated, 
## Identify time of event and reduce to 24 hours before event and 24 hours after event
## Create Interval variable to indicate time before or after event

# This function will assign the time of event as the first instance of severe sepsis
# It will then assign the time before or after event for all other observations
z.1 <- function(dataset){
  # Identify time of event
  x <- dataset$charttime[which(dataset$Severe_Sepsis==1)[1]]    # x = time of event
#   dataset <- dataset[abs(difftime(x, dataset$charttime, units="hours"))<=6,]
#   for (i in 1:nrow(dataset))
#     dataset$Interval[i] <- difftime(dataset$charttime[i], x, units="hours")
  return(x)
}

## Apply function z.1 to set time of event for all other observations with severe sepsis by definition 1
severe.sepsis.all <- severe.sepsis[order(severe.sepsis$id, severe.sepsis$charttime), ]
severe.sepsis.timeofevent <- ddply(severe.sepsis.all, .(id), z.1)
names(severe.sepsis.timeofevent) <- c('id', 'time.event')
save(severe.sepsis.timeofevent, file = "severe.sepsis.timeofevent.RData")

# load("severe.sepsis.timeofevent.RData")
## Apply function z.1 to set time of event for all other observations with severe sepsis by definition 6
severe.sepsis2.timeofevent <- ddply(severe.sepsis2, .(id), z.1)
names(severe.sepsis2.timeofevent) <- c('id', 'time.event')
save(severe.sepsis2.timeofevent, file = "severe.sepsis2.timeofevent.RData")

load('severe.sepsis.timeofevent.RData')
severe.sepsis.all <- severe.sepsis.data[order(severe.sepsis.data$id, severe.sepsis.data$charttime), ]
severe.sepsis.all.2 <- merge(severe.sepsis.all, severe.sepsis.timeofevent, by.x = 'id', by.y = 'id', all.x = T)
save(severe.sepsis.all.2, file = "severe.sepsis.all.2.RData")
# write.csv(severe.sepsis.intervals, "severe.sepsis.intervals.csv", row.names=FALSE)
severe.sepsis2.all.2 <- merge(severe.sepsis2.all, severe.sepsis2.timeofevent, by.x = 'id', by.y = 'id', all.x = T)
save(severe.sepsis2.all.2, file = "severe.sepsis2.all.2.RData")



f.1b <- function (x) {
  if (!is.na(x[2]) & (!is.na(x[8])) & (difftime(x[8], x[2], units = 'hours') > 2 ) & (difftime(x[8], x[2], units = 'hours') <= 24)) {
    t <- 1
  }
  else
  {
    t <- 0
  }
}

severe.sepsis.all.2$includerow <- apply(severe.sepsis.all.2, 1, f.1b) # recognize rows with charttime between -24<x<-2
head(severe.sepsis.all.2)  



f.1c <- function (x) {
  if (!is.na(x[2]) & (!is.na(x[8])) & (difftime(x[8], x[2], units = 'hours') > 2 ) & (difftime(x[8], x[2], units = 'hours') <= 8)) {
    t <- 1
  }
  else
  {
    t <- 0
  }
}

severe.sepsis.all.2$includerow <- apply(severe.sepsis.all.2, 1, f.1b)
severe.sepsis.all.2$includerow2 <- apply(severe.sepsis.all.2, 1, f.1c) # recognize rows with charttime between -8<x<-2
head(severe.sepsis.all.2)  
save(severe.sepsis.all.2, file = 'severe.sepsis.all.2.RData')

# target group data
target <- severe.sepsis.all.2[severe.sepsis.all.2$includerow > 0,]
target.id <- unique(target$id) #734 patients
write.csv(target, "target_24hrs.csv", row.names=FALSE)

target2 <- severe.sepsis.all.2[severe.sepsis.all.2$includerow2 > 0,]
target2.id <- unique(target2$id) #
write.csv(target2, "target_8hrs.csv", row.names=FALSE)



#================================control group==================================
# control group - hl, no bc w/in 24 hr period; no hl, bc w/in 24 hr period; neither hl or bc w/in 24 hr period
control.group <- complete[!complete$id %in% hl.bc, ]
control.group <- control.group[!is.na(control.group$charttime),]
library(plyr)
control.group <- control.group[order(control.group$id, control.group$charttime),]

## Convert HALFHOUR to Date/Time format
library(lubridate)
control.group$charttime <- ymd_hms(control.group$charttime)

# start times and end times for each patient
start.time <- ddply(control.group, "id", summarise, min(charttime))
end.time <- ddply(control.group, "id", summarise, max(charttime))

start.time2 <- start.time[,2] + 3600 * 8
for (i in 1:nrow(start.time))
  if (difftime(end.time[i,2], start.time[i,2], units = 'hours') < 8) {
    start.time2[i] <- start.time[i,2]
  }

# 
# pool <- data.frame(start.time[,1], start.time2, end.time2)
pool <- data.frame(start.time[,1], start.time2, end.time[,2])
names(pool) <- c("id", "start.time", "end.time")
pool <- pool[!(is.na(pool$start.time) | is.na(pool$end.time)),]  
pool$diff <- pool$end.time - pool$start.time
set.seed(12345)
random.time <- sapply(1:nrow(pool), function(i) runif(1, min = 0, max =pool[i,4]))
pool$event.time <- pool$start.time+as.period(random.time,unit="seconds")

control.all <- merge(control.group, pool, by.x = 'id', by.y = 'id', all.x = T)
control.all.2 <- control.all[order(control.all$id, control.all$charttime),]
head(control.all.2)


f.1b2 <- function (x) {
  if (!is.na(x[2]) & (!is.na(x[11])) & (difftime(x[11], x[2], units = 'hours') > 2 ) & (difftime(x[11], x[2], units = 'hours') <= 24)) {
    t <- 1
  }
  else
  {
    t <- 0
  }
}

f.1c2 <- function (x) {
  if (!is.na(x[2]) & (!is.na(x[11])) & (difftime(x[11], x[2], units = 'hours') > 2 ) & (difftime(x[11], x[2], units = 'hours') <= 8)) {
    t <- 1
  }
  else
  {
    t <- 0
  }
}
control.all.2$includerow <- apply(control.all.2, 1, f.1b2)
control.all.2$includerow2 <- apply(control.all.2, 1, f.1c2) # recognize rows with charttime between -8<x<-2
head(control.all.2)  

# target group data
control <- control.all.2[control.all.2$includerow > 0,]
control.id <- unique(control$id) #734 patients
write.csv(control, "control_24hrs.csv", row.names=FALSE)

control2 <- control.all.2[control.all.2$includerow2 > 0,]
control2.id <- unique(control2$id) #734 patients
write.csv(control2, "control_8hrs.csv", row.names=FALSE)



#============================merge datasets to get control and target group==============
# Import Data ----------------
target <- read.csv("target_8hrs.csv", header = T)
target01 <- read.csv("target_24hrs.csv", header = T)
control <- read.csv("control_8hrs.csv", header = T)

names(target)
names(control)
length(unique(target$id))
length(unique(control$id))

target2 <- target[,c(1:4, 8)]
names(target2) <- c('id', 'charttime', 'itemid', 'valuenum', 'event.time')

control2 <- control[,c(1:4, 11)]
names(control2) <- c('id', 'charttime', 'itemid', 'valuenum', 'event.time')

target2$Response <- rep(1, nrow(target2))
control2$Response <- rep(0, nrow(control2))

length(unique(target2$id))
length(unique(control2$id))

data.all <- rbind(target2, control2)
write.csv(data.all, 'alldata_8hrs.csv')


#=================Capping the data===============================
# library(plyr)

data.all <- read.csv('alldata_8hrs.csv', header = T)

data0 <- data.all
# Convert to percentiles
bounds = function(x, lower.bound, upper.bound)
{
  check.lower <- x < lower.bound
  check.upper <- x > upper.bound
  x[check.lower] <- lower.bound
  x[check.upper] <- upper.bound
  return (x)
}

dropped.variables <- as.list(c(50009, 50025, 50006, 50419, 50395, 50184, 50429, 50399, 50012, 50316, 677))
data00 <- data0[!(data0$itemid %in% dropped.variables),]

percentile = function(parameter){
  w <- data00[data00$itemid==parameter,]
  lower.bound = quantile(w$valuenum, c(.01), na.rm=TRUE)
  upper.bound = quantile(w$valuenum, c(.99), na.rm=TRUE)
  w$meanvalue <- bounds(w$valuenum, lower.bound, upper.bound)
  return (w)          
}


data00 <- data00[!is.na(data00$itemid),]


GCS <- data00[data00$itemid==198 & !(is.na(data00$itemid)),]
GCS$meanvalue <- GCS$valuenum
data00.capped <- rbind(GCS,percentile(211),percentile(455),percentile(618),
                       percentile(646),percentile(678),percentile(733),percentile(763),
                       percentile(4552),percentile(50002),percentile(50007),percentile(50010),
                       percentile(50013),percentile(50015),percentile(50016),percentile(50018),
                       percentile(50019),percentile(50060),percentile(50061),percentile(50062),
                       percentile(50068),percentile(50073),percentile(50079),percentile(50090),
                       percentile(50091),percentile(50112),percentile(50122),percentile(50140),
                       percentile(50148),percentile(50149),percentile(50159),percentile(50170),
                       percentile(50171),percentile(50172),percentile(50177),percentile(50188),
                       percentile(50383),percentile(50386),percentile(50428),percentile(50439),
                       percentile(50440),percentile(50451),percentile(50468))


data00.capped <- data00.capped[!is.na(data00.capped$meanvalue),]

data00.capped <- data00.capped[order(data00.capped$id, data00.capped$charttime),]

write.csv(data00.capped, "data.capped.csv", row.names=FALSE)

#=================
# library(reshape2)

data00.capped <- read.csv('data.capped.csv', header = T)

#To be continue from here.........................................................
# table(data00.capped$Response)


# library(plyr)
data00.capped.id <- unique(data00.capped$id)

# generate.variables <- function(data00.capped1) {
#   data1.labs <- data00.capped1[data00.capped1$itemid %in% lab.id,]
#   
#   # Derive features for each variable: median, standard deviation, minimum, maximum
#   labs.1 <- ddply(data1.labs, .(id, itemid), 
#                   function(x){y = data.frame(x$id[1],x$itemid[1], 
#                                              median(x$meanvalue, na.rm=T),
#                                              sd(x$meanvalue, na.rm=T),
#                                              min(x$meanvalue, na.rm=T),
#                                              max(x$meanvalue, na.rm=T)
#                   );
#                   names(y)= c("id", "itemid", "median", "std", "min", "max");
#                   # names(y)= c("id", "itemid", "max");
#                   return(y)})
#   # convert infinity to NA
#   labs.1 <- do.call(data.frame,lapply(labs.1, function(x) replace(x, is.infinite(x),NA)))
#   
#   # transform to wide format
#   # Here you can add more features if you want. Just need to rename those variables before merging
#   data.wide.median.l1 <- dcast(labs.1, id~itemid, value.var= "MEDIAN")
#   data.wide.std.l1 <- dcast(labs.1, id~itemid, value.var= "STD")
#   data.wide.min.l1 <- dcast(labs.1, id~itemid, value.var= "MIN")
#   data.wide.max.l1 <- dcast(labs.1, id~itemid, value.var= "MAX")
#   names(data.wide.median.l1) <- paste(names(data.wide.median.l1), "MEDIAN", sep=".")
#   names(data.wide.std.l1) <- paste(names(data.wide.std.l1), "STD", sep=".")
#   names(data.wide.min.l1) <- paste(names(data.wide.min.l1), "MIN", sep=".")
#   names(data.wide.max.l1) <- paste(names(data.wide.max.l1), "MAX", sep=".")
#   # Merge data frames. remove subject ID columns except the first one
#   data.wide.l1 <- data.frame(data.wide.median.l1, data.wide.std.l1[,-1], data.wide.min.l1[,-1],data.wide.max.l1[,-1], check.names=F)
#   
# }

generate.variables2 <- function(data00.capped1) {
  
  # Laboratory Values
  
  # Selected variables for labs (excludes any obtained from blood gas)
  lab.id <- sort(c(455,4552,211,678,618,50060,50061,50068,50062,50073,50172,50177,50079,
                   50090,50091,50013,50112,50386,50383,50140,
                   50148,50428,50468,50122,50188))
  
  columnnames <- c("id.C", "211.C","455.C", "618.C", "678.C",  "4552.C", "50013.C", "50060.C", "50061.C", "50062.C", "50068.C", "50073.C", "50079.C", "50090.C", "50091.C", "50112.C", "50122.C", 
                   "50140.C", "50148.C", "50172.C", "50177.C", "50188.C", "50383.C", "50386.C", "50428.C", "50468.C", "211.B","455.B", "618.B", "678.B",  "4552.B", "50013.B", "50060.B", 
                   "50061.B", "50062.B", "50068.B", "50073.B", "50079.B", "50090.B", "50091.B", "50112.B", "50122.B", "50140.B", "50148.B", "50172.B",
                   "50177.B", "50188.B", "50383.B", "50386.B", "50428.B", "50468.B", "211.A","455.A", "618.A", "678.A",  "4552.A", "50013.A", "50060.A", "50061.A", "50062.A", "50068.A", 
                   "50073.A", "50079.A", "50090.A", "50091.A", "50112.A", "50122.A", "50140.A", "50148.A", "50172.A", "50177.A", "50188.A", "50383.A", 
                   "50386.A", "50428.A", "50468.A", "211.CB","455.CB", "618.CB", "678.CB",  "4552.CB", "50013.CB", "50060.CB", "50061.CB", "50062.CB", "50068.CB", "50073.CB", "50079.CB", "50090.CB",
                   "50091.CB", "50112.CB", "50122.CB", "50140.CB", "50148.CB", "50172.CB", "50177.CB", "50188.CB", "50383.CB", "50386.CB", "50428.CB", "50468.CB","211.BA","455.BA", "618.BA", "678.BA",  "4552.BA", 
                   "50013.BA", "50060.BA", "50061.BA", "50062.BA", "50068.BA", "50073.BA", "50079.BA", "50090.BA", "50091.BA", "50112.BA", "50122.BA", "50140.BA", 
                   "50148.BA", "50172.BA", "50177.BA", "50188.BA", "50383.BA", "50386.BA", "50428.BA", "50468.BA")
  
  data1.labs <- data00.capped1[data00.capped1$itemid %in% lab.id,]
  
  # Derive features for each variable: median, standard deviation, minimum, maximum
  labs.1 <- ddply(data1.labs, .(id, itemid), 
                  function(x){ if (nrow(x) >=3) {y = data.frame(x$id[1],x$itemid[1],x$meanvalue[nrow(x)],
                                                                x$meanvalue[nrow(x)-1],x$meanvalue[nrow(x)-2],   
                                                                x$meanvalue[nrow(x)] - x$meanvalue[nrow(x)-1], 
                                                                x$meanvalue[nrow(x)-1] - x$meanvalue[nrow(x)-2])}
                    else if (nrow(x) >= 2) {y = data.frame(x$id[1],x$itemid[1],x$meanvalue[nrow(x)],
                                                           x$meanvalue[nrow(x)-1], NA,
                                                           x$meanvalue[nrow(x)] - x$meanvalue[nrow(x)-1], NA)}
                    
                    else {y = data.frame(x$id[1],x$itemid[1],x$meanvalue[nrow(x)], NA, NA, NA, NA)};
                    names(y)= c("id", "itemid", "C", "B", "A", "CB", "BA");
                    # names(y)= c("id", "itemid", "max");
                    return(y)})
  # convert infinity to NA
  labs.1 <- do.call(data.frame,lapply(labs.1, function(x) replace(x, is.infinite(x),NA)))

  # transform to wide format
  # Here you can add more features if you want. Just need to rename those variables before merging
  data.wide.C.l1 <- dcast(labs.1, id~itemid, value.var= "C")
  data.wide.B.l1 <- dcast(labs.1, id~itemid, value.var= "B")
  data.wide.A.l1 <- dcast(labs.1, id~itemid, value.var= "A")
  data.wide.CB.l1 <- dcast(labs.1, id~itemid, value.var= "CB")
  data.wide.BA.l1 <- dcast(labs.1, id~itemid, value.var= "BA")
  names(data.wide.C.l1) <- paste(names(data.wide.C.l1), "C", sep=".")
  names(data.wide.B.l1) <- paste(names(data.wide.B.l1), "B", sep=".")
  names(data.wide.A.l1) <- paste(names(data.wide.A.l1), "A", sep=".")
  names(data.wide.CB.l1) <- paste(names(data.wide.CB.l1), "CB", sep=".")
  names(data.wide.BA.l1) <- paste(names(data.wide.BA.l1), "BA", sep=".")
  # Merge data frames. remove subject ID columns except the first one
  data.wide.l1 <- data.frame(data.wide.C.l1, data.wide.B.l1[,-1], data.wide.A.l1[,-1],data.wide.CB.l1[,-1],data.wide.BA.l1[,-1], check.names=F)
  data.wide.l2 <- data.wide.l1
  for (j in (1:length(columnnames)))
    if (!(columnnames[j] %in% colnames(data.wide.l1))) {
      data.wide.l2[columnnames[j]] <- NA
    }
  return(data.wide.l2)
}



data.features <- matrix(NA,nrow = 0,ncol = 126)
colnames(data.features) <-  c("id.C", "211.C","455.C", "618.C", "678.C",  "4552.C", "50013.C", "50060.C", "50061.C", "50062.C", "50068.C", "50073.C", "50079.C", "50090.C", "50091.C", "50112.C", "50122.C", 
                             "50140.C", "50148.C", "50172.C", "50177.C", "50188.C", "50383.C", "50386.C", "50428.C", "50468.C", "211.B","455.B", "618.B", "678.B",  "4552.B", "50013.B", "50060.B", 
                             "50061.B", "50062.B", "50068.B", "50073.B", "50079.B", "50090.B", "50091.B", "50112.B", "50122.B", "50140.B", "50148.B", "50172.B",
                             "50177.B", "50188.B", "50383.B", "50386.B", "50428.B", "50468.B", "211.A","455.A", "618.A", "678.A",  "4552.A", "50013.A", "50060.A", "50061.A", "50062.A", "50068.A", 
                             "50073.A", "50079.A", "50090.A", "50091.A", "50112.A", "50122.A", "50140.A", "50148.A", "50172.A", "50177.A", "50188.A", "50383.A", 
                             "50386.A", "50428.A", "50468.A", "211.CB","455.CB", "618.CB", "678.CB",  "4552.CB", "50013.CB", "50060.CB", "50061.CB", "50062.CB", "50068.CB", "50073.CB", "50079.CB", "50090.CB",
                             "50091.CB", "50112.CB", "50122.CB", "50140.CB", "50148.CB", "50172.CB", "50177.CB", "50188.CB", "50383.CB", "50386.CB", "50428.CB", "50468.CB","211.BA","455.BA", "618.BA", "678.BA",  "4552.BA", 
                             "50013.BA", "50060.BA", "50061.BA", "50062.BA", "50068.BA", "50073.BA", "50079.BA", "50090.BA", "50091.BA", "50112.BA", "50122.BA", "50140.BA", 
                             "50148.BA", "50172.BA", "50177.BA", "50188.BA", "50383.BA", "50386.BA", "50428.BA", "50468.BA")
i = 3 #make i = 3
size = 500
while (i * size < length(data00.capped.id)) 
  {
  if ((i+1) * size <= length(data00.capped.id)) {
  data00.cappedi <- data00.capped[data00.capped$id %in% data00.capped.id[(i * size + 1) : ((i + 1) * size)],]
  }
  else
  {data00.cappedi <- data00.capped[data00.capped$id %in% data00.capped.id[(i * size + 1) : (length(data00.capped.id))],]}
  i = i + 1
  data <- generate.variables2(data00.cappedi)
  write.csv(data, file = paste(i,'data.csv', sep = '_'))
  data.features <- rbind(data.features, data)
}
write.csv(data.features, 'data_features.csv')

# data.features <- read.csv('1_data.csv', header = T)
# data.features$X <- NULL
# columnnames = names(data.features)
# for (j in (2:i)) {
#   data_j <- read.csv(paste(j,'data.csv', sep = '_'), header = T)
#   data_j$X <- NULL
#   data_j2 <- data_j
#   for (j in (1:length(columnnames))) {
#     if (!(columnnames[j] %in% colnames(data_j))) {
#       # nm = columnnames[j]
#       data_j2[columnnames[j]] <- NA
#     if ('nm' %in% colnames(data_j)) {data_j2$nm <- NULL}
#     }
#   }
#   data.features <- rbind(data.features, data_j2)
# }
  
# install.packages("stringr")
library(stringr)
hospids <- function(ids) {
  ids2 <- lapply(ids, function (x) str_sub(x, 1, -5))
  ids2 <- unlist(ids2)
  return(ids2)
}

data00.capped.responseinfo <- unique(data.frame(data00.capped$id, data00.capped$Response))
names(data00.capped.responseinfo) <- c('id','Response')
data00.capped.responseinfo$id2 <- hospids(data00.capped.responseinfo$id)
  
catheters <- read.csv('pts_catheters.csv', header = T)
names(catheters)

icuseqs_catheter <- read.csv('icuseqs_catheters.csv',header = F)
catheters$icuseq <- as.vector(unlist(icuseqs_catheter), mode = 'numeric')

catheters$id <- paste(catheters$subject_id, catheters$hospital_seq, catheters$icuseq, sep='#%#')
catheters.bin <- data.frame(unique(catheters$id))
names(catheters.bin) <- c('id')
catheters.bin$cath <- 1

data00.capped.info <- merge(data00.capped.responseinfo, catheters.bin, by.x = 'id', by.y = 'id', all.x = T)
data00.capped.info$cath[is.na(data00.capped.info$cath)] <- 0


comorbids <- read.csv('pts_comorbids_new.csv', header = T)
names(comorbids)
comorbids$score <- apply(comorbids[,-c(1,2,3)], 1, sum)
comorbids$id2 <- paste(comorbids$subject_id, comorbids$hospital_seq, sep='#%#')
comorbids2 <- data.frame(comorbids$id2, comorbids$score)
names(comorbids2) <- c('id2', 'score')
data00.capped.info <- merge(data00.capped.info, comorbids2, by.x = 'id2', by.y = 'id2', all.x = T)

data.featurevectors <- merge(data.features, data00.capped.info, by.x = 'id.C', by.y = 'id', all.x = T)
data.featurevectors$id.C <- as.character(data.featurevectors$id.C)
hospid <- apply(data.featurevectors$id.C, 1, function(x) strsplit(x, "#%#")[2])
data.featurevectors$hospid <- unlist(strsplit(data.featurevectors$id.C, "#%#"))[2]

data.featurevectors$hosp1[data.featurevectors$hospid == '1'] <- 'Y'
data.featurevectors$hosp1[data.featurevectors$hospid != '1'] <- 'N'

write.csv(data.featurevectors, 'data.featurevectors.csv')

data.featurevectors2 <- data.featurevectors
names(data.featurevectors2)
data.featurevectors2$id2 <- NULL
data.featurevectors2$hospid <- NULL
write.csv(data.featurevectors2, 'data.featurevectors2.csv')


# Drop features that have less than 50% of data
num.na <- apply(data.featurevectors2,2,function(x){return(length(which(is.na(x))))})
data.featurevectors3 <- data.featurevectors2[,num.na<nrow(data.featurevectors2)/2]

# Drop observations that have less than 50% of data
obs.na <- apply(data.featurevectors3,1,function(x){return(length(which(is.na(x))))})
data.featurevectors3 <- data.featurevectors3[obs.na<(ncol(data.featurevectors3)-2)/2,]
names(data.featurevectors3)
#write.csv(num.na, "missing.csv", row.names=F)
# Impute

install.packages('DMwR')
require(DMwR)
table(data.featurevectors3$Response)
str(data.featurevectors3)
head(data.featurevectors3)
names(data.featurevectors3)
data.featurevectors3$cath <- as.vector(as.character(data.featurevectors3$cath))
data.featurevectors3$Response <- as.vector(as.character(data.featurevectors3$Response))
data.featurevectors3$hosp1 <- as.vector(data.featurevectors3$hosp1)

data.featurevectors4 <- knnImputation(data.featurevectors3[,-c(1,23,24,26)], k=50, scale=T, meth="weighAvg")
data.featurevectors4$id <- data.featurevectors3$id.C
data.featurevectors4$cath <- data.featurevectors3$cath
data.featurevectors4$hosp1 <- data.featurevectors3$hosp1
data.featurevectors4$Response <- data.featurevectors3$Response
write.csv(data.featurevectors4, file = 'data.featurevectors4.csv')


#============feature selection based on info gain==================================
data.featurevectors4 <- read.csv("data.featurevectors4.csv", header = T)
data.featurevectors4$X <- NULL

install.packages("FSelector")
library(FSelector)
data.featurevectors5 <- data.featurevectors4[,-23] #column of pt id
weights <- information.gain(Response~., data.featurevectors5)
print(weights)

#==============================others========================================
for (i in 1:nrow(severe.sepsis.all))
  if (!is.na(severe.sepsis.all$spec_itemid[i]))
    severe.sepsis.all$itemid[i] <-severe.sepsis.all$spec_itemid[i]

# Aggregate data (item) by HALFHOUR
write.csv(severe.sepsis.all, 'severe_sepsis_all.csv')
halfhours1 <- read.csv("halfhours_severe_sepsis_all.csv", header = F)
severe.sepsis.all$HALFHOUR <- as.vector(unlist(halfhours1))
for (i in 1:nrow(severe.sepsis.all))
  if (!is.na(severe.sepsis.all$spec_itemid[i]))
    severe.sepsis.all$itemid[i] <-severe.sepsis.all$spec_itemid[i]

# severe.sepsis.all <- severe.sepsis.all[!is.na(severe.sepsis.all$HALFHOUR),]
meanvalues <-aggregate(severe.sepsis.all$valuenum, by=list(severe.sepsis.all$id,severe.sepsis.all$itemid,severe.sepsis.all$HALFHOUR), 
                       FUN=mean, na.rm=TRUE)

names(meanvalues) <- c('id', 'itemid', 'halfhours', 'meanvalue')


