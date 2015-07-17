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

lab$comorb <- read.csv('comorbscore_labs.csv', header = F)
lab$comorb <- as.vector(unlist(lab$comorb), mode = 'numeric')

bld$comorb <- read.csv('comorbscore_blds.csv', header = F)
bld$comorb <- as.vector(unlist(bld$comorb), mode = 'numeric')

charts$comorb <- read.csv('comorbscore_vitals.csv', header = F)
charts$comorb <- as.vector(unlist(charts$comorb), mode = 'numeric')

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
highlactates <- lactates[lactates$valuenum >= 4,] 

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
length(reduced.sepsis$id[!duplicated(reduced.sepsis$id)]) # 2,183
### Function for above code 

z.0 <- function(complete) {
  #complete <- data[data$Sid==patient,]
  ## Identify rows with blood culture taken
  for (i in 1:nrow(complete))
    if (!is.na(complete$itemid[i]) & (complete$itemid[i] == 70011 | complete$itemid[i] == 70012))
      complete$Blood_Culture[i] <- 1 
    ## Identify rows with high lactate
    for (i in 1:nrow(complete))
      if(!is.na(complete$itemid[i]) & !is.na(complete$valuenum[i]) & (complete$itemid[i] == 50010 | complete$itemid[i] == 818 | complete$itemid[i] == 1531) & complete$valuenum[i] >= 4)
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
save(severe.sepsis, file = 'severe.sepsis.RData')


# bc.missing.sepsis.1 <- possible.sepsis[(possible.sepsis$id %in% bc.missing.patients),]
# # Subset of blood culture with time (excludes blood culture without time)
# bc.missing.sepsis.2 <- bc.missing.sepsis.1[!is.na(bc.missing.sepsis.1$itemid) & (bc.missing.sepsis.1$itemid == 70011 | bc.missing.sepsis.1$itemid == 70012) & !(is.na(bc.missing.sepsis.1$charttime)),]
# 
# ## Identify patients who have at least one blood culture with a chart time: 395
# bc.patients.0 <- bc.missing.sepsis.2[!duplicated(bc.missing.sepsis.2$id),1]
# 
# ## Subset of data for patients who have at least one blood culture with a chart time
# bc.missing.sepsis.3 <- bc.missing.sepsis.1[bc.missing.sepsis.1$id %in% bc.patients.0,]
# # Choose only values with chart time
# bc.missing.sepsis.4 <- bc.missing.sepsis.3[!is.na(bc.missing.sepsis.3$charttime),]
# 
# ## Apply function z.0 to create data set with proper labels for Blood Culture, High Lactate, and Severe Sepsis
# # bc.missing.sepsis.5 <- bc.missing.sepsis.4[!is.na(bc.missing.sepsis.4$spec_itemid) | bc.missing.sepsis.4$itemid==50010,]
# data.2 <- ddply(bc.missing.sepsis.4, .(id), z.0) 
# 
# ## Identify first instance of high lactate for each patient (from 'data.2' - patients previously excluded)
# hl.subset.0 <- data.2[data.2$High_Lactate==1,]                         # Returns all instances of high lactate
# hl.subset.1 <- hl.subset.0[!duplicated(hl.subset.0$id),]
# 
# ## Identify patients whose first high lactate is also a severe sepsis event
# hl.subset.2 <- hl.subset.1[hl.subset.1$Severe_Sepsis==1,]
# additional.ss.index <- hl.subset.2[!duplicated(hl.subset.2$id), 1] #230 patients
# 
# ## If a patient's first high lactate was also a severe sepsis event, add that patients data to the Severe Sepsis group
# additional.ss <- data.2[(data.2$id %in% additional.ss.index),]
# severe.sepsis.all <- rbind(severe.sepsis, additional.ss)                  # Combine with 'severe.sepsis' which is the first dataset that originally excluded the second group
# length(severe.sepsis.all$id[!duplicated(severe.sepsis.all$id)]) #1,373 patients
# severe.sepsis.id <- severe.sepsis.all$id[!duplicated(severe.sepsis.all$id)]

#==========================analyze the time from admission to ICU to onset of severe sepsis==================
ptsinfos2 <- read.csv('pticu_infos.csv', header = T)
ptsinfos2$id <- paste(ptsinfos2$subject_id, ptsinfos2$hospital_seq, ptsinfos2$icustay_seq, sep='#%#')
names(ptsinfos2)
icuadms <- data.frame(ptsinfos2$id, ptsinfos2$icustay_intime)
names(icuadms) <- c('id', 'intime')

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

hist(severe.sepsis.eventtime.2$dftime, xlim = c(0,100))
hist(severe.sepsis.eventtime.2$dftime, xlim = c(0,24))
summary(severe.sepsis.eventtime.2$dftime)
boxplot(severe.sepsis.eventtime.2$dftime)

severe.sepsis.eventtime.3 <- severe.sepsis.eventtime.2[order(severe.sepsis.eventtime.2$dftime),]
save(severe.sepsis.eventtime.3, file = 'severe.sepsis.eventtime.3.RData')


d <- density(severe.sepsis.eventtime.3$dftime)
plot(d, main = 'Time from ICU admission to onset of severe sepsis')
d <- density(severe.sepsis.eventtime.3$dftime[1:1100])
plot(d, main = 'Time from ICU admission to onset of severe sepsis[1:1100')
hist(severe.sepsis.eventtime.3$dftime, main = 'Time from ICU admission to onset of severe sepsis')
hist(severe.sepsis.eventtime.3$dftime[1:1100], probability = T, main = 'Time from ICU admission to onset of severe sepsis[1:1100]', ylim=c(0,0.25))
lines(density(severe.sepsis.eventtime.3$dftime[1:1100]))  
boxplot(severe.sepsis.eventtime.3$dftime)
boxplot(severe.sepsis.eventtime.3$dftime[1:1100])
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

## Apply function z.1 to set time of event and set intervals for all other observations
severe.sepsis.timeofevent <- ddply(severe.sepsis.all, .(id), z.1)
names(severe.sepsis.timeofevent) <- c('id', 'time.event')
severe.sepsis.all.2 <- merge(severe.sepsis.all, severe.sepsis.timeofevent, by.x = 'id', by.y = 'id', all.x = T)
write.csv(severe.sepsis.intervals, "severe.sepsis.intervals.csv", row.names=FALSE)

# severe.sepsis.intime <- data.frame(severe.sepsis.eventtime.3$id, severe.sepsis.eventtime.3$intime)
# names(severe.sepsis.intime) <- c('id', 'intime')
# severe.sepsis.all.3 <- merge(severe.sepsis.all.2, severe.sepsis.intime, by.x = 'id', by.y = 'id', all.x = T)
# 
# severe.sepsis.all.3$left.time0 <- severe.sepsis.all.3$time.event - 10800*2*4
# severe.sepsis.all.3$left.time <- apply(severe.sepsis.all.3[,c(9,10)], 1, max)
# severe.sepsis.all.3$left.time <- as.POSIXct(severe.sepsis.all.3$left.time,format="%Y-%m-%d %H:%M:%S")

# names(severe.sepsis.all.3)

# f.1a <- function (x) {
#   if (!is.na(x[2]) & !is.na(x[11]) & !is.na(x[8]) & (x[2] >= x[11]) & (x[2] < x[8])) {
#     t <- 1
#   }
#   else
#   {
#     t <- 0
#   }
# }

# severe.sepsis.all.3$includerow <- apply(severe.sepsis.all.3, 1, f.1a) # recognize rows with charttime between -6<x<0


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

# target group data
target <- severe.sepsis.all.2[severe.sepsis.all.2$includerow > 0,]
target.id <- unique(target$id)
write.csv(target, "target_6hrs.csv", row.names=FALSE)


f.1c <- function (x) {
  if (!is.na(x[2]) & (!is.na(x[8])) & (difftime(x[8], x[2], units = 'hours') > 2 ) & (difftime(x[8], x[2], units = 'hours') <= 6)) {
    t <- 1
  }
  else
  {
    t <- 0
  }
}

severe.sepsis.all.2$includerow2 <- apply(severe.sepsis.all.2, 1, f.1c) # recognize rows with charttime between -6<x<-2
head(severe.sepsis.all.2)  

# target group data
target <- severe.sepsis.all.2[severe.sepsis.all.2$includerow > 0,]
target.id <- unique(target$id) #734 patients
write.csv(target, "target_24hrs.csv", row.names=FALSE)

target2 <- severe.sepsis.all.2[severe.sepsis.all.2$includerow2 > 0,]
target2.id <- unique(target2$id) #
write.csv(target2, "target_6hrs.csv", row.names=FALSE)



#================================control group==================================
# control group - hl, no bc w/in 24 hr period; no hl, bc w/in 24 hr period; neither hl or bc w/in 24 hr period
control.group <- complete[!complete$id %in% hl.bc, ]

library(plyr)
control.group <- control.group[order(control.group$id, control.group$charttime),]

## Convert HALFHOUR to Date/Time format
library(lubridate)
control.group$charttime <- ymd_hms(control.group$charttime)

# start times and end times for each patient
start.time <- ddply(control.group, "id", summarise, min(charttime))
end.time <- ddply(control.group, "id", summarise, max(charttime))

# start.time2 <- start.time[,2]+10800 # 43200 seconds = 12 hours;
# end.time2 <- end.time[,2]-10800
# 
# 
# pool <- data.frame(start.time[,1], start.time2, end.time2)
pool <- data.frame(start.time[,1], start.time[,2], end.time[,2])
names(pool) <- c("id", "start.time", "end.time")
pool <- pool[!(is.na(pool$start.time) | is.na(pool$end.time)),]  
pool$diff <- pool$end.time - pool$start.time

control.all <- merge(control.group, pool$diff, by.x = 'id', by.y = 'id', all.x = T)


control.all.2 <- control.all[control.all$diff > 3600 * 24, ]
set.seed(12345)

random.time <- sapply(1:nrow(pool), function(i) runif(1, 0, max = pool[i, 4]))
pool$middle.time <- pool$start.time+as.period(random.time,unit="seconds")
head(pool)

minutes <- floor( as.numeric( substr( pool$middle.time, 15, 16 ) ) / 30 ) * 30
minutes <- ifelse( minutes == 30, "30", "00" )
seconds <- rep ("00", nrow(pool))


pool$middle.time <- as.character( pool$middle.time )
substr( pool$middle.time , 15, 16 ) <- minutes
substr( pool$middle.time , 18, 19 ) <- seconds
pool$middle.time <- as.POSIXct( pool$middle.time,format="%Y-%m-%d %H:%M:%S")
head( pool)


pool$left.time0 <- pool$middle.time - 10800*2
pool$left.time <- apply(pool[,c(2,6)], 1, max)
pool$right.time <- pool$middle.time #time of event
pool$left.time <- as.POSIXct(pool$left.time,format="%Y-%m-%d %H:%M:%S")


head(pool)
write.csv(pool, "pool.csv")

# pool2 <- pool[,c(1,5)]
# head(pool2)
# write.csv(pool2, "pool2.csv")

# Format control group and include time0
control <- pool
control.group$charttime <- as.POSIXct(control.group$charttime,format="%Y-%m-%d %H:%M:%S")
control.group.2 <- control.group[!is.na(control.group$charttime),]
control.group.2$charttime <- as.character(control.group.2$charttime)
write.csv(control.group.2$charttime, 'control.group.charttime2.csv')

halfhours1 <- read.csv("halfhours_control_group_charttime.csv", header = F)
control.group.2$HALFHOUR <- as.vector(unlist(halfhours1))

# control.lab.2 <- control.lab[!is.na(control.lab$charttime) | control.lab$charttime =='NA',]
# control.lab.2$charttime <- as.character(control.lab.2$charttime)
# write.csv(control.lab.2$charttime, 'control_lab_charttime2.csv')
# halfhours1 <- read.csv("halfhours_control_lab_charttime.csv", header = F)
# control.lab.2$HALFHOUR <- as.vector(unlist(halfhours1))
# severe.sepsis.all <- severe.sepsis.all[!is.na(severe.sepsis.all$HALFHOUR),]
meanvalues <-aggregate(control.group.2$valuenum, by=list(control.group.2$id, control.group.2$itemid, control.group.2$HALFHOUR), 
                       FUN=mean, na.rm=TRUE)
names(meanvalues) <- c('id', 'itemid', 'halfhours', 'meanvalue')

control.lab <- merge(meanvalues, control, by.x = 'id', by.y = 'id', all.x = T)

names(control.lab)
f.1 <- function (x) {
  if (!is.na(x[3]) & !is.na(x[10]) & !is.na(x[11]) & (x[3] >= x[10]) & (x[3] < x[11])) {
    t <- 1
  }
  else
  {
    t <- 0
  }
}

control.lab$includerow <- apply(control.lab, 1, f.1)
head(control.lab)  
# lab control
lab.control <- control.lab[control.lab$includerow > 0,]
write.csv(lab.control, "lab.control_6hrs.csv", row.names=FALSE)


## Chart events for control group

chart1.cg <- chart1[(chart1$id %in% control$id),]
# chart1.cg$time0 <- rep(0, nrow(chart1.cg))
chart1.cg$subject_id <- NULL
chart1.cg$icustay_id <- NULL
chart1.cg$icustay_seq <- NULL

names(chart1.cg) <- c("charttime","itemid","valuenum","hospital_seq", "comorb" ,"id")
control2 <- data.frame(chart1.cg$charttime, chart1.cg$itemid, chart1.cg$valuenum, chart1.cg$hospital_seq, chart1.cg$id)
names(control2) <- c("charttime","itemid","valuenum","hospital_seq", "id")

control2$charttime <- as.POSIXct(control2$charttime,format="%Y-%m-%d %H:%M:%S")
control.2 <- control2[!is.na(control2$charttime),]
control.2$charttime <- as.character(control.2$charttime)
write.csv(control.2$charttime, 'control.charttime2.csv')

halfhours2 <- read.csv("halfhours_control_charttime.csv", header = F)
control.2$HALFHOUR <- as.vector(unlist(halfhours2))

# control.lab.2 <- control.lab[!is.na(control.lab$charttime) | control.lab$charttime =='NA',]
# control.lab.2$charttime <- as.character(control.lab.2$charttime)
# write.csv(control.lab.2$charttime, 'control_lab_charttime2.csv')
# halfhours1 <- read.csv("halfhours_control_lab_charttime.csv", header = F)
# control.lab.2$HALFHOUR <- as.vector(unlist(halfhours1))
# severe.sepsis.all <- severe.sepsis.all[!is.na(severe.sepsis.all$HALFHOUR),]
meanvalues.vitals <-aggregate(control.2$valuenum, by=list(control.2$id, control.2$itemid, control.2$HALFHOUR), 
                       FUN=mean, na.rm=TRUE)
names(meanvalues.vitals) <- c('id', 'itemid', 'halfhours', 'meanvalue')

control.vitals <- merge(meanvalues.vitals, control, by.x = 'id', by.y = 'id', all.x = T)
control.vitals$includerow <- apply(control.vitals, 1, f.1)
head(control.vitals)  
# lab control
vitals.control <- control.vitals[control.vitals$includerow > 0,]
# chart control
write.csv(vitals.control, "vitals.control_6hrs.csv", row.names=FALSE)

#To do:
#1. to aggregrate target group data into halfhours
#2. to merge comborbidity and hospital_seq data
#3. get ready for prediction models

chart.target <- read.csv("chart.target.csv", header=T)
chart.target$charttime <- as.POSIXct(chart.target$charttime,format="%Y-%m-%d %H:%M:%S")
chart.target.2 <- chart.target[!is.na(chart.target$charttime),]
chart.target.2$charttime <- as.character(chart.target.2$charttime)
write.csv(chart.target.2$charttime, 'target.charttime2.csv')

halfhours3 <- read.csv("halfhours_target_charttime.csv", header = F)
chart.target.2$HALFHOUR <- as.vector(unlist(halfhours3))

meanvalues.vitals.target <-aggregate(chart.target.2$valuenum, by=list(chart.target.2$id, chart.target.2$itemid, chart.target.2$HALFHOUR), 
                              FUN=mean, na.rm=TRUE)
names(meanvalues.vitals.target) <- c('id', 'itemid', 'halfhours', 'meanvalue')
write.csv(meanvalues.vitals.target, "meanvalues.vitals.target_6hrs.csv", row.names=FALSE)

#===============lab.target to aggregate to halfhours data=============================
lab.target <- read.csv("lab.target.csv", header=T)
lab.target$charttime <- as.POSIXct(lab.target$charttime,format="%Y-%m-%d %H:%M:%S")
lab.target.2 <- lab.target[!is.na(lab.target$charttime),]
lab.target.2$charttime <- as.character(lab.target.2$charttime)
write.csv(lab.target.2$charttime, 'lab.target.charttime2.csv')

halfhours4 <- read.csv("halfhours_lab_target_charttime.csv", header = F)
lab.target.2$HALFHOUR <- as.vector(unlist(halfhours4))

meanvalues.lab.target <-aggregate(lab.target.2$valuenum, by=list(lab.target.2$id, lab.target.2$itemid, lab.target.2$HALFHOUR), 
                                     FUN=mean, na.rm=TRUE)
names(meanvalues.lab.target) <- c('id', 'itemid', 'halfhours', 'meanvalue')
write.csv(meanvalues.lab.target, "meanvalues.lab.target_6hrs.csv", row.names=FALSE)

length(unique(meanvalues.lab.target$id)) #1,128
length(unique(meanvalues.vitals.target$id)) #597


#============================merge datasets to get control and target group==============
# Import Data ----------------
lab.target <- read.csv("meanvalues.lab.target_6hrs.csv", header = T)
chart.control <- read.csv("vitals.control_6hrs.csv", header = T)
chart.target <- read.csv("meanvalues.vitals.target_6hrs.csv", header = T)
lab.control <- read.csv("lab.control_6hrs.csv", header = T)

names(lab.target)
names(lab.control)
names(chart.target)
names(chart.control)


lab.control.2 <- lab.control[,c(1:4)]
chart.control.2 <- chart.control[,c(1:4)]

controldata <- rbind(lab.control.2, chart.control.2)
controldata$Response <- rep(0, nrow(controldata))
targetdata <- rbind(lab.target, chart.target)
targetdata$Response <- rep(1, nrow(targetdata))
length(unique(controldata$id))
length(unique(targetdata$id))
write.csv(controldata, 'controldata_6hrs.csv')
write.csv(targetdata, 'targetdata_6hrs.csv')
data.all <- rbind(targetdata, controldata)
write.csv(data.all, 'alldata_6hrs.csv')

#=================Capping the data===============================
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
  lower.bound = quantile(w$meanvalue, c(.01), na.rm=TRUE)
  upper.bound = quantile(w$meanvalue, c(.99), na.rm=TRUE)
  w$meanvalue <- bounds(w$meanvalue, lower.bound, upper.bound)
  return (w)          
}


data00 <- data00[!is.na(data00$itemid),]
GCS <- data00[data00$itemid==198 & !(is.na(data00$itemid)),]

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

data00.capped <- data00.capped[order(data00.capped$id, data00.capped$halfhours),]

write.csv(data00.capped, "data.capped.csv", row.names=FALSE)

#=================
data00.capped <- read.csv('data.capped.csv', header = T)

controldata <- read.csv("controldata_6hrs.csv", header = T)
targetdata <- read.csv("targetdata_6hrs.csv", header = T)

table(data00.capped$Response)

# Laboratory Values

# Selected variables for labs (excludes any obtained from blood gas)
lab.id <- c(50060,50061,50068,50062,50073,50172,50177,50079,
            50090,50091,50013,50112,50386,50383,50140,
            50148,50428,50468,50122,50188)
data1.labs <- data00.capped[data00.capped$itemid %in% lab.id,]

# Derive features for each variable: median, standard deviation, minimum, maximum
labs.1 <- ddply(data1.labs, .(id, itemid), 
                function(x){y = data.frame(x$id[1],x$itemid[1],
                                           median(x$meanvalue, na.rm=T),
                                           sd(x$meanvalue, na.rm=T),
                                           min(x$meanvalue, na.rm=T),
                                           max(x$meanvalue, na.rm=T)
                );
                names(y)= c("id", "itemid", "median", "std", "min", "max");
                return(y)})
# convert infinity to NA
labs.1 <- do.call(data.frame,lapply(labs.1, function(x) replace(x, is.infinite(x),NA)))

# transform to wide format
# Here you can add more features if you want. Just need to rename those variables before merging
data.wide.median.l1 <- dcast(labs.1, id~itemid, value.var= "MEDIAN")
data.wide.std.l1 <- dcast(labs.1, id~itemid, value.var= "STD")
data.wide.min.l1 <- dcast(labs.1, id~itemid, value.var= "MIN")
data.wide.max.l1 <- dcast(labs.1, id~itemid, value.var= "MAX")
names(data.wide.median.l1) <- paste(names(data.wide.median.l1), "MEDIAN", sep=".")
names(data.wide.std.l1) <- paste(names(data.wide.std.l1), "STD", sep=".")
names(data.wide.min.l1) <- paste(names(data.wide.min.l1), "MIN", sep=".")
names(data.wide.max.l1) <- paste(names(data.wide.max.l1), "MAX", sep=".")

# Merge data frames. remove subject ID columns except the first one
data.wide.l1 <- data.frame(data.wide.median.l1, data.wide.std.l1[,-1], data.wide.min.l1[,-1],data.wide.max.l1[,-1], check.names=F)














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


