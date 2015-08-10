#Prepare patient data in target group and control group, respectively.

#Read lab, bld culture, and vital signs data
lab <- read.csv("pts_lab_new.csv", header=TRUE)
bld<- read.csv("pts_bldcultures3_new.csv", header=TRUE)
charts <- read.csv('pts_vitals_new.csv')

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


bld1 <- data.frame(bld$id, bld$charttime, bld$spec_itemid, bld$hospital_seq, bld$comorb)
bld1$valuenum <- rep(NA, nrow(bld1))
names(bld1) <- c('id', 'charttime', 'itemid', 'hospital_seq', 'comorb', 'valuenum')

lab1 <- data.frame(lab$id, lab$charttime, lab$hospital_seq, lab$itemid, lab$valuenum, lab$comorb)
names(lab1) <- c('id', 'charttime', 'hospital_seq', 'itemid', 'valuenum', 'comorb')


#combine the lab and bld culture data
complete <- rbind(lab1, bld1) 

library(lubridate)
complete$charttime <- ymd_hms(complete$charttime)
complete <- complete[order(complete$charttime),]

## Add indicator columns to data frame
complete$Blood_Culture <- c(rep(0,nrow(complete)))
complete$High_Lactate <- c(rep(0,nrow(complete)))
complete$Severe_Sepsis <- c(rep(0,nrow(complete)))

#find patients with high lactate value
lactates <- complete[!is.na(complete$itemid) & complete$itemid == 50010,]       # lactate lab values
length(table(lactates$id))          # total patients had lactate tested  #24,358
highlactates <- lactates[lactates$valuenum >= 4,] 

hl <- highlactates$id[!duplicated(highlactates$id) & !is.na(highlactates$id)]            # Identify individuals with a high lactate
#2740 patients

bldcultures <- complete[!is.na(complete$itemid) & (complete$itemid == 70011 | complete$itemid == 70012),] 
bc <- bldcultures$id[!duplicated(bldcultures$id) & !is.na(bldcultures$id)]     # Identify patients with a blood culture taken (12,845)

## From 'complete' data set, select only patients who have high lactate and blood culture
hl.bc <- intersect(hl, bc)  #2,039
# hl.bc <- as.list(hl.bc)

possible.sepsis <- complete[(complete$id %in% hl.bc),] 
# Identify patients in 'possible.sepsis' data set who have missing chart times for blood culture
# bc.missing <- possible.sepsis[possible.sepsis$HALFHOUR=="" & !is.na(possible.sepsis$SPEC_ITEMID),]         # Try this command if the one right below does not work
bc.missing <- possible.sepsis[is.na(possible.sepsis$charttime) & (possible.sepsis$itemid == 70011 | possible.sepsis$itemid == 70012),]      
bc.missing.patients <- bc.missing[!duplicated(bc.missing$id),1] #417 pts

## Create reduced subset of 'complete' that excludes patients without chart time for blood culture
reduced.sepsis <- possible.sepsis[!(possible.sepsis$id %in% bc.missing.patients),]

# Count how many patients in 'reduced.sepsis' : 
length(reduced.sepsis$id[!duplicated(reduced.sepsis$id)]) #1,622
### Function for above code 

z.0 <- function(complete){
  #complete <- data[data$Sid==patient,]
  ## Identify rows with blood culture taken
  for (i in 1:nrow(complete))
    if (!is.na(complete$itemid[i]) & (complete$itemid[i] == 70011 | complete$itemid[i] == 70012))
      complete$Blood_Culture[i] <- 1 
    ## Identify rows with high lactate
    for (i in 1:nrow(complete))
      if(!is.na(complete$itemid[i]) & !is.na(complete$valuenum[i]) & complete$itemid[i] == 50010 & complete$valuenum[i] >=4)
        complete$High_Lactate[i] <- 1
      ## Identify rows with severe sepsis
      # Empty list for chart times for blood culture
      time.bc <- c()
      # Get chart times for blood culture
      for (i in 1:nrow(complete)) 
        if (complete$Blood_Culture[i] == 1)
          time.bc <- c((complete$charttime[i]), time.bc)
      time.bc <- rev(time.bc)
      # Assign severe sepsis to rows with high lactate that occur within 24 hours of a blood culture
      for (i in 1:nrow(complete))
        if (complete$High_Lactate[i] == 1 & min(abs(difftime(time.bc, complete$charttime[i], units="hours"))) < 24)
          complete$Severe_Sepsis[i] <- 1
      return(complete)
}


library(plyr)
## Apply function z.0 to create data set with proper labels for Blood Culture, High Lactate, and Severe Sepsis
data1 <- ddply(reduced.sepsis, .(id), z.0) 

## Identify patients from 'data1' who have severe sepsis
severe.sepsis.observations <- data1[data1$Severe_Sepsis==1,]
severe.sepsis.pt <- severe.sepsis.observations[!duplicated(severe.sepsis.observations$id),1]

## Count number of patients classified with severe.sepsis: 1,123; 1143
length(severe.sepsis.pt)

## Create new data set with patients who have severe sepsis
severe.sepsis <- data1[data1$id %in% severe.sepsis.pt,]

bc.missing.sepsis.1 <- possible.sepsis[(possible.sepsis$id %in% bc.missing.patients),]
# Subset of blood culture with time (excludes blood culture without time)
bc.missing.sepsis.2 <- bc.missing.sepsis.1[!is.na(bc.missing.sepsis.1$itemid) & (bc.missing.sepsis.1$itemid == 70011 | bc.missing.sepsis.1$itemid == 70012) & !(is.na(bc.missing.sepsis.1$charttime)),]

## Identify patients who have at least one blood culture with a chart time: 395
bc.patients.0 <- bc.missing.sepsis.2[!duplicated(bc.missing.sepsis.2$id),1]

## Subset of data for patients who have at least one blood culture with a chart time
bc.missing.sepsis.3 <- bc.missing.sepsis.1[bc.missing.sepsis.1$id %in% bc.patients.0,]
# Choose only values with chart time
bc.missing.sepsis.4 <- bc.missing.sepsis.3[!is.na(bc.missing.sepsis.3$charttime),]

## Apply function z.0 to create data set with proper labels for Blood Culture, High Lactate, and Severe Sepsis
# bc.missing.sepsis.5 <- bc.missing.sepsis.4[!is.na(bc.missing.sepsis.4$spec_itemid) | bc.missing.sepsis.4$itemid==50010,]
data.2 <- ddply(bc.missing.sepsis.4, .(id), z.0) 

## Identify first instance of high lactate for each patient (from 'data.2' - patients previously excluded)
hl.subset.0 <- data.2[data.2$High_Lactate==1,]                         # Returns all instances of high lactate
hl.subset.1 <- hl.subset.0[!duplicated(hl.subset.0$id),]

## Identify patients whose first high lactate is also a severe sepsis event
hl.subset.2 <- hl.subset.1[hl.subset.1$Severe_Sepsis==1,]
additional.ss.index <- hl.subset.2[!duplicated(hl.subset.2$id), 1] #230 patients

## If a patient's first high lactate was also a severe sepsis event, add that patients data to the Severe Sepsis group
additional.ss <- data.2[(data.2$id %in% additional.ss.index),]
severe.sepsis.all <- rbind(severe.sepsis, additional.ss)                  # Combine with 'severe.sepsis' which is the first dataset that originally excluded the second group
length(severe.sepsis.all$id[!duplicated(severe.sepsis.all$id)]) #1,373 patients
severe.sepsis.id <- severe.sepsis.all$id[!duplicated(severe.sepsis.all$id)]

#==========================analyze the time from admission to ICU to onset of severe sepsis
mortalityinfos2 <- read.csv('pticu_infos.csv', header = T)
mortalityinfos2$id <- paste(mortalityinfos2$subject_id, mortalityinfos2$hospital_seq, mortalityinfos2$icustay_seq, sep='#%#')
names(mortalityinfos2)
icuadms <- data.frame(mortalityinfos2$id, mortalityinfos2$icustay_intime)
names(icuadms) <- c('id', 'intime')

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
d <- density(severe.sepsis.eventtime.3$dftime[1:1000])
plot(d)
hist(severe.sepsis.eventtime.3$dftime[1:1000])

## For patients with Severe Sepsis indicated, 
## Identify time of event and reduce to 24 hours before event and 24 hours after event
## Create Interval variable to indicate time before or after event

# This function will assign the time of event as the first instance of severe sepsis
# It will then assign the time before or after event for all other observations
z.1 <- function(dataset){
  # Identify time of event
  x <- dataset$charttime[which(dataset$Severe_Sepsis==1)[1]]    # x = time of event
  dataset <- dataset[abs(difftime(x, dataset$charttime, units="hours"))<=6,]
  for (i in 1:nrow(dataset))
    dataset$Interval[i] <- difftime(dataset$charttime[i], x, units="hours")
  return(dataset)
}

## Apply function z.1 to set time of event and set intervals for all other observations
severe.sepsis.intervals <- ddply(severe.sepsis.all, .(id), z.1)
write.csv(severe.sepsis.intervals, "severe.sepsis.intervals.csv", row.names=FALSE)

## Histogram of intervals
hist(severe.sepsis.intervals$Interval)

# severe.sepsis.intervals <- read.csv("severe.sepsis.intervals.csv", header=T)
## New data set with values before time of event (necessary for prediction)
severe.sepsis.intervals.2 <- severe.sepsis.intervals[severe.sepsis.intervals$Interval < 0, c(1:6)]


target.complete <- severe.sepsis.intervals.2
target.complete$time0 <- rep(1, nrow(target.complete))


# Get diastolic blood pressure
bp.value2 <- charts[charts$itemid == 455,]
bp.value2[,c(6)] <- bp.value2[,c(7)]
bp.value2$itemid <- 4552  
# assign dbp to new put back into chart1
chart1 <- data.frame(rbind(charts, bp.value2))
# drop meanvalue2 and stdvalue2
chart1$value2num <- NULL


# temperature in F
temp.C <- chart1[chart1$itemid == 678,]
chart1$charttime <- ymd_hms(chart1$charttime)
names(chart1)
chart2 <- data.frame(chart1$id, chart1$charttime, chart1$hospseq, chart1$itemid, chart1$value1num, chart1$comorb)
names(chart2) <- c('id', 'charttime', 'hospital_seq', 'itemid', 'valuenum', 'comorb')
z.2 <- function(dataset){
  # Identify time of event
  x <- dataset$charttime[which(dataset$time0==1)]    # x = time of event
  dataset <- dataset[abs(difftime(x, dataset$charttime, units="hours"))<=6,]
  for (i in 1:nrow(dataset))
    dataset$Interval[i] <- difftime(dataset$charttime[i], x, units="hours")
  return(dataset)
}

## Chart events for target group
# find patients that have severe sepsis
chart2.sp <- chart2[chart2$id %in% hl.bc,]
chart2.sp$time0 <- rep(0, nrow(chart2.sp))

chart2.sp.time <- rbind(target.complete, chart2.sp)
chart2.sp.time.2 <- chart2.sp.time[!is.na(chart2.sp.time$charttime),]       # Remove NA values (no NA values)
chart.sp.intervals <- ddply(chart2.sp.time.2, .(id), z.2) 
hist(chart.sp.intervals$Interval)

chart.target <- chart.sp.intervals[chart.sp.intervals$Interval < 0,]
write.csv(chart.target, "chart.target.csv", row.names=FALSE)
write.csv(target.complete, "lab.target.csv", row.names=FALSE)



#================================control group==================================
# control group - hl, no bc w/in 24 hr period; no hl, bc w/in 24 hr period; neither hl or bc w/in 24 hr period
control.group <- complete[!complete$id %in% severe.sepsis.id, ]

library(plyr)
control.group <- control.group[order(control.group$id, control.group$charttime),]

## Convert HALFHOUR to Date/Time format
library(lubridate)
control.group$charttime <- ymd_hms(control.group$charttime)

# start times and end times for each patient
start.time <- ddply(control.group, "id", summarise, min(charttime))
end.time <- ddply(control.group, "id", summarise, max(charttime))

start.time2 <- start.time[,2]+10800 # 43200 seconds = 12 hours;
end.time2 <- end.time[,2]-10800


pool <- data.frame(start.time[,1], start.time2, end.time2)
names(pool) <- c("id", "start.time", "end.time")
pool <- pool[!(is.na(pool$start.time) | is.na(pool$end.time)),]  

pool$diff <- pool$end.time - pool$start.time
set.seed(12345)
random.time <- sapply(1:nrow(pool), function(i) runif(1, 0, max = pool[i,4]))
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

pool$left.time <- pool$middle.time - 10800
pool$right.time <- pool$middle.time + 10800

head(pool)
write.csv(pool, "pool.csv")

pool2 <- pool[,c(1,5)]
head(pool2)
write.csv(pool2, "pool2.csv")

# Format control group and include time0
control <- pool2
control$middle.time <- ymd_hms(control$middle.time)
control$time0 <- rep(1, nrow(control))
control$hospital_seq <- rep(NA, nrow(control))
control$itemid <- rep(NA, nrow(control))
control$valuenum <- rep(NA, nrow(control))
control$comorb <- rep(NA, nrow(control))
control$X <- NULL

names(control)[names(control)=="middle.time"] <- "charttime"
control$charttime <- ymd_hms(control$charttime)
control$Blood_Culture <- rep(NA, nrow(control)) 
control$High_Lactate <- rep(NA, nrow(control)) 
control$Severe_Sepsis <- rep(NA, nrow(control)) 

# Control group lab events and bld cultures events
control.group$time0 <- rep(0, nrow(control.group))
control.event.time <- rbind(control, control.group)
control.event.time.2 <- control.event.time[!is.na(control.event.time$charttime),]      # Remove NA values
length(unique(control.event.time.2$id))

control.intervals <- ddply(control.event.time.2, .(id), z.2) 

# lab control

lab.control <- control.intervals[control.intervals$Interval < 0,]
write.csv(lab.control, "lab.control.csv", row.names=FALSE)


## Chart events for control group

chart1.cg <- chart1[(chart1$id %in% control$id),]
chart1.cg$time0 <- rep(0, nrow(chart1.cg))
chart1.cg$subject_id <- NULL
chart1.cg$icustay_id <- NULL
chart1.cg$icustay_seq <- NULL

names(chart1.cg) <- c("charttime", 'ITEMID', "MEANVALUE1", "STDVALUE1", 'HSEQ', "ID", "COMORB", "time0")
control2 <- data.frame(control$HALFHOUR, control$ITEMID, control$MEANVALUE, control$STDVALUE, control$HSEQ, control$ID, control$COMORB, control$time0)
names(control2) <- c("HALFHOUR", 'ITEMID', "MEANVALUE1", "STDVALUE1", 'HSEQ', "ID", "COMORB", "time0")
chart1.cg.time <- rbind(control2, chart1.cg)
chart1.cg.time.2 <- chart1.cg.time[!is.na(chart1.cg.time$HALFHOUR),]       # Remove NA values
chart.cg.intervals <- ddply(chart1.cg.time.2, .(ID), z.2) 

# chart control

chart.control <- chart.cg.intervals[chart.cg.intervals$Interval < 0,]
write.csv(chart.control, "chart.control.csv", row.names=FALSE)



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
