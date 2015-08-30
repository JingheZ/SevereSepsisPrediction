#To analyze lactate decrease and mortality among severe sepsis patients 

library(lubridate)
library(plyr)

#general mortality info===================================================
mortalityinfos2 <- read.csv('pticu_infos.csv', header = T)
mortalityinfos2$id <- paste(mortalityinfos2$subject_id, mortalityinfos2$hospital_seq, mortalityinfos2$icustay_seq, sep='#%#')
names(mortalityinfos2)
length(unique(mortalityinfos2$id)) #24200
mortalityinfos2$id0 <- paste(mortalityinfos2$subject_id, mortalityinfos2$hospital_seq, sep='#%#')
length(unique(mortalityinfos2$id0)) #22756
mortalityinfos2$hospital_disch_dt <- ymd_hms(mortalityinfos2$hospital_disch_dt)
mortalityinfos2$icustay_outtime <- ymd_hms(mortalityinfos2$icustay_outtime)
mortalityinfos2$dod <- ymd_hms(mortalityinfos2$dod)

hosp_mortality <- mortalityinfos2[!is.na(mortalityinfos2$hospital_expire_flg) & mortalityinfos2$hospital_expire_flg == 'Y',]
hosp_mortality.id <- unique(hosp_mortality$id) #3,028
hosp_mortality.id2 <- unique(hosp_mortality$id0) #2,733

icu_mortality <- mortalityinfos2[!is.na(mortalityinfos2$icustay_expire_flg) & mortalityinfos2$icustay_expire_flg == 'Y',]
icu_mortality.id <- unique(icu_mortality$id) # 2,026


load("severe.sepsis.timeofevent.RData")
df1.timeofevent <- severe.sepsis.timeofevent
df1.mortality30 <- merge(df1.timeofevent, mortalityinfos2, by.x = 'id', by.y = 'id', all.x = T)
df1.mortality30 <- df1.mortality30[!is.na(df1.mortality30$dod),]
df1.mortality30.id <- c()
for (i in 1:nrow(df1.mortality30)) 
  if (difftime(df1.mortality30$dod[i], df1.mortality30$time.event[i], units="hours") < 30*24) {
    df1.mortality30.id <- c(df1.mortality30.id, as.character(df1.mortality30$id[i]))
  }

load("severe.sepsis2.timeofevent.RData")
df6.timeofevent <- severe.sepsis2.timeofevent
df6.mortality30 <- merge(df6.timeofevent, mortalityinfos2, by.x = 'id', by.y = 'id', all.x = T)
df6.mortality30 <- df6.mortality30[!is.na(df6.mortality30$dod),]
head(df6.mortality30)
df6.mortality30.id <- c()
for (i in 1:nrow(df6.mortality30)) 
  if (difftime(df6.mortality30$dod[i], df6.mortality30$time.event[i], units="hours") < 30*24) {
    df6.mortality30.id <- c(df6.mortality30.id, as.character(df6.mortality30$id[i]))
  }


# install.packages("stringr")
library(stringr)
hospids <- function(ids) {
  ids2 <- lapply(ids, function (x) str_sub(x, 1, -5))
  ids2 <- unique(unlist(ids2))
  return(ids2)
}


#===================================lactate changes for sepsis patients under definition 1==================================
# severe sepsis patients with lactate > 4 by definition 1
load("severe.sepsis.all.2.RData") 
head(severe.sepsis.all.2)
severe.sepsis.all.2 <- severe.sepsis.all.2[order(severe.sepsis.all.2$id, severe.sepsis.all.2$charttime),]
head(severe.sepsis.all.2)

#remove those items before time of event (severe sepsis) and only keep those after developed severe sepsis
severe.sepsis.all.3 <- severe.sepsis.all.2[difftime(severe.sepsis.all.2$charttime, severe.sepsis.all.2$time.event, units = 'hours') >= 0, ]
lactates <- severe.sepsis.all.3[(severe.sepsis.all.3$itemid == 50010 | severe.sepsis.all.3$itemid == 818 | severe.sepsis.all.3$itemid == 1531),]
head(lactates)
lactates <- lactates[!is.na(lactates$valuenum),]
lactates$lactate.timediff <- difftime(lactates$charttime, lactates$time.event, units = 'hours')

#get the first lactate test and analyze the time of the first lactate test from severe sepsis
lactate.first <- data.frame(matrix(ncol = 0, nrow = 0))
f.lactate.begin <- function(dataset){
  # Identify time of event
  first <- dataset[1,]
  lactate.first  <- rbind(lactate.first, first)
  return(lactate.first)
}

#all the first lactate is the time of event
lactates.first0 <- ddply(lactates, .(id), f.lactate.begin) 

#get the last lactate test and analyze the time of the last lactate test from severe sepsis
lactate.last <- data.frame(matrix(ncol = 0, nrow = 0))
f.lactate.end <- function(dataset){
  # Identify time of event
  len <- dim(dataset)[1]
  last <- dataset[len,]
  lactate.last  <- rbind(lactate.last, last)
  return(lactate.last)
}
#all the first lactate is the time of event
lactates.last0 <- ddply(lactates, .(id), f.lactate.end) 

d <- density(as.numeric(lactates.last0$lactate.timediff))
plot(d, main = 'Time of last lactate test from time of event')
boxplot(as.numeric(lactates.last0$lactate.timediff))
summary(as.numeric(lactates.last0$lactate.timediff))


lactates.last1 <- lactates.last0[order(lactates.last0$lactate.timediff),]
d <- density(as.numeric(lactates.last1$lactate.timediff[1:1200]))
plot(d, main = 'Time of last lactate test from time of event[1:1200]')
boxplot(as.numeric(lactates.last1$lactate.timediff[1:1200]))
summary(as.numeric(lactates.last1$lactate.timediff[1:1200]))

#====================lactate change from time of event to the first 6 hours========================================
#get the lactate test by every 6 hours
lactate.first6 <- data.frame(matrix(ncol = 0, nrow = 0))
f.lactate.first6 <- function(dataset){
  # Identify time of event
  first6 <- dataset[(abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 6) == min(abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 6))) & (abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 6) <= 2), ][1,]
  lactate.first6  <- rbind(lactate.first6, first6)
}

lactate.first6 <- ddply(lactates, .(id), f.lactate.first6) 
lactate.first6$initial.lactate <- lactates.first0$valuenum
lactate.first6 <- lactate.first6[!is.na(lactate.first6$id), ]

lactates.2 <- lactate.first6
head(lactates.2)

#for each patient,analyze whether lactate level decrease by 10% and lactate normalized
lactates.id <- unique(lactates.2$id)
lactate.change <- data.frame(matrix(ncol = 7, nrow = length(lactates.id)))
names(lactate.change) <- c('id', 'first.time', 'first.valuenum','last.time', 'last.valuenum', 'decrease_10', 'normalized')
lactate.change$id <- lactates.id
lactate.change$first.time <- ymd_hms(lactate.change$first.time)
lactate.change$last.time <- ymd_hms(lactate.change$last.time)

for (i in 1:length(lactates.id)) {
  if (lactates.2[i, 4] <= 0.9 * lactates.2[i, 10]) {
    decrease10 <- 'Y'
  }
  else {decrease10 <- 'N'}
  
  if (lactates.2[i, 4] <= 2.5) {
    normalization <- 'Y'
  }
  else {normalization <- 'N'}  

  lactate.change$first.time[i] <- lactates.2[i, 8]
  lactate.change$last.time[i] <- lactates.2[i, 2]
  lactate.change$first.valuenum[i] <- lactates.2[i, 10]
  lactate.change$last.valuenum[i] <- lactates.2[i, 4]
  lactate.change$decrease_10[i] <- decrease10
  lactate.change$normalized[i] <- normalization
}


lactate.decrease10 <- lactate.change[lactate.change$decrease_10 == 'Y',]
lactate.non_decrease10 <- lactate.change[lactate.change$decrease_10 == 'N',]
lactate.normalization <- lactate.change[lactate.change$normalized == 'Y',]
lactate.non_normalization <- lactate.change[lactate.change$normalized == 'N',]


lactate.decrease10.id <- unique(lactate.decrease10$id)
lactate.non_decrease10.id <- unique(lactate.non_decrease10$id) 
lactate.normalization.id <- unique(lactate.normalization$id) 
lactate.non_normalization.id <- unique(lactate.non_normalization$id)


lactate.decrease10.id2 <- hospids(lactate.decrease10.id) 
lactate.non_decrease10.id2 <- hospids(lactate.non_decrease10.id) 
lactate.normalization.id2 <- hospids(lactate.normalization.id) 
lactate.non_normalization.id2 <- hospids(lactate.non_normalization.id) 


lactate.decrease10.hosp_mortality <- intersect(lactate.decrease10.id, hosp_mortality.id) 
lactate.non_decrease10.hosp_mortality <- intersect(lactate.non_decrease10.id, hosp_mortality.id)
lactate.normalization.hosp_mortality <- intersect(lactate.normalization.id, hosp_mortality.id) 
lactate.non_normalization.hosp_mortality <- intersect(lactate.non_normalization.id, hosp_mortality.id) 



lactate.decrease10.icu_mortality <- intersect(lactate.decrease10.id, icu_mortality.id) 
lactate.non_decrease10.icu_mortality <- intersect(lactate.non_decrease10.id, icu_mortality.id) 
lactate.normalization.icu_mortality <- intersect(lactate.normalization.id, icu_mortality.id) 
lactate.non_normalization.icu_mortality <- intersect(lactate.non_normalization.id, icu_mortality.id) 


lactate.decrease10.mortality30 <- intersect(lactate.decrease10.id, df1.mortality30.id)
lactate.non_decrease10.mortality30 <- intersect(lactate.non_decrease10.id, df1.mortality30.id) 
lactate.normalization.mortality30 <- intersect(lactate.normalization.id, df1.mortality30.id) 
lactate.non_normalization.mortality30 <- intersect(lactate.non_normalization.id, df1.mortality30.id) 


length(lactate.decrease10.icu_mortality) / length(lactate.decrease10.id)
length(lactate.decrease10.hosp_mortality) / length(lactate.decrease10.id2)
length(lactate.decrease10.mortality30) / length(lactate.decrease10.id)


length(lactate.non_decrease10.icu_mortality) / length(lactate.non_decrease10.id)
length(lactate.non_decrease10.hosp_mortality) / length(lactate.non_decrease10.id2)
length(lactate.non_decrease10.mortality30) / length(lactate.non_decrease10.id)

length(lactate.normalization.icu_mortality) / length(lactate.normalization.id)
length(lactate.normalization.hosp_mortality) / length(lactate.normalization.id2)
length(lactate.normalization.mortality30) / length(lactate.normalization.id)


length(lactate.non_normalization.icu_mortality) / length(lactate.non_normalization.id)
length(lactate.non_normalization.hosp_mortality) / length(lactate.non_normalization.id2)
length(lactate.non_normalization.mortality30) / length(lactate.non_normalization.id)



#====================lactate change from time of event to the first 12 hours========================================
#get the lactate test by every 6 hours
lactate.first12 <- data.frame(matrix(ncol = 0, nrow = 0))
f.lactate.first12 <- function(dataset){
  # Identify time of event
  first12 <- dataset[(abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 12) == min(abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 12))) & (abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 12) <= 2), ][1,]
  lactate.first12  <- rbind(lactate.first12, first12)
}

lactate.first12 <- ddply(lactates, .(id), f.lactate.first12) 
lactate.first12$initial.lactate <- lactates.first0$valuenum
lactate.first12 <- lactate.first12[!is.na(lactate.first12$id), ]

lactates.2 <- lactate.first12
head(lactates.2)

#for each patient,analyze whether lactate level decrease by 10% and lactate normalized
lactates.id <- unique(lactates.2$id)
lactate.change <- data.frame(matrix(ncol = 7, nrow = length(lactates.id)))
names(lactate.change) <- c('id', 'first.time', 'first.valuenum','last.time', 'last.valuenum', 'decrease_10', 'normalized')
lactate.change$id <- lactates.id
lactate.change$first.time <- ymd_hms(lactate.change$first.time)
lactate.change$last.time <- ymd_hms(lactate.change$last.time)

for (i in 1:length(lactates.id)) {
  if (lactates.2[i, 4] <= 0.9 * lactates.2[i, 10]) {
    decrease10 <- 'Y'
  }
  else {decrease10 <- 'N'}
  
  if (lactates.2[i, 4] <= 2.5) {
    normalization <- 'Y'
  }
  else {normalization <- 'N'}  
  
  lactate.change$first.time[i] <- lactates.2[i, 8]
  lactate.change$last.time[i] <- lactates.2[i, 2]
  lactate.change$first.valuenum[i] <- lactates.2[i, 10]
  lactate.change$last.valuenum[i] <- lactates.2[i, 4]
  lactate.change$decrease_10[i] <- decrease10
  lactate.change$normalized[i] <- normalization
}


lactate.decrease10 <- lactate.change[lactate.change$decrease_10 == 'Y',]
lactate.non_decrease10 <- lactate.change[lactate.change$decrease_10 == 'N',]
lactate.normalization <- lactate.change[lactate.change$normalized == 'Y',]
lactate.non_normalization <- lactate.change[lactate.change$normalized == 'N',]


lactate.decrease10.id <- unique(lactate.decrease10$id)
lactate.non_decrease10.id <- unique(lactate.non_decrease10$id) 
lactate.normalization.id <- unique(lactate.normalization$id) 
lactate.non_normalization.id <- unique(lactate.non_normalization$id)


lactate.decrease10.id2 <- hospids(lactate.decrease10.id) 
lactate.non_decrease10.id2 <- hospids(lactate.non_decrease10.id) 
lactate.normalization.id2 <- hospids(lactate.normalization.id) 
lactate.non_normalization.id2 <- hospids(lactate.non_normalization.id) 


lactate.decrease10.hosp_mortality <- intersect(lactate.decrease10.id, hosp_mortality.id) 
lactate.non_decrease10.hosp_mortality <- intersect(lactate.non_decrease10.id, hosp_mortality.id)
lactate.normalization.hosp_mortality <- intersect(lactate.normalization.id, hosp_mortality.id) 
lactate.non_normalization.hosp_mortality <- intersect(lactate.non_normalization.id, hosp_mortality.id) 


lactate.decrease10.icu_mortality <- intersect(lactate.decrease10.id, icu_mortality.id) 
lactate.non_decrease10.icu_mortality <- intersect(lactate.non_decrease10.id, icu_mortality.id) 
lactate.normalization.icu_mortality <- intersect(lactate.normalization.id, icu_mortality.id) 
lactate.non_normalization.icu_mortality <- intersect(lactate.non_normalization.id, icu_mortality.id) 


lactate.decrease10.mortality30 <- intersect(lactate.decrease10.id, df1.mortality30.id)
lactate.non_decrease10.mortality30 <- intersect(lactate.non_decrease10.id, df1.mortality30.id) 
lactate.normalization.mortality30 <- intersect(lactate.normalization.id, df1.mortality30.id) 
lactate.non_normalization.mortality30 <- intersect(lactate.non_normalization.id, df1.mortality30.id) 


length(lactate.decrease10.icu_mortality) / length(lactate.decrease10.id)
length(lactate.decrease10.hosp_mortality) / length(lactate.decrease10.id2)
length(lactate.decrease10.mortality30) / length(lactate.decrease10.id)


length(lactate.non_decrease10.icu_mortality) / length(lactate.non_decrease10.id)
length(lactate.non_decrease10.hosp_mortality) / length(lactate.non_decrease10.id2)
length(lactate.non_decrease10.mortality30) / length(lactate.non_decrease10.id)

length(lactate.normalization.icu_mortality) / length(lactate.normalization.id)
length(lactate.normalization.hosp_mortality) / length(lactate.normalization.id2)
length(lactate.normalization.mortality30) / length(lactate.normalization.id)


length(lactate.non_normalization.icu_mortality) / length(lactate.non_normalization.id)
length(lactate.non_normalization.hosp_mortality) / length(lactate.non_normalization.id2)
length(lactate.non_normalization.mortality30) / length(lactate.non_normalization.id)



#====================lactate change from time of event to the first 18 hours========================================
#get the lactate test by every 6 hours
lactate.first18 <- data.frame(matrix(ncol = 0, nrow = 0))
f.lactate.first18 <- function(dataset){
  # Identify time of event
  first18 <- dataset[(abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 18) == min(abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 18))) & (abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 18) <= 2), ][1,]
  lactate.first18  <- rbind(lactate.first18, first18)
}

lactate.first18 <- ddply(lactates, .(id), f.lactate.first18) 
lactate.first18$initial.lactate <- lactates.first0$valuenum
lactate.first18 <- lactate.first18[!is.na(lactate.first18$id), ]

lactates.2 <- lactate.first18
head(lactates.2)

#for each patient,analyze whether lactate level decrease by 10% and lactate normalized
lactates.id <- unique(lactates.2$id)
lactate.change <- data.frame(matrix(ncol = 7, nrow = length(lactates.id)))
names(lactate.change) <- c('id', 'first.time', 'first.valuenum','last.time', 'last.valuenum', 'decrease_10', 'normalized')
lactate.change$id <- lactates.id
lactate.change$first.time <- ymd_hms(lactate.change$first.time)
lactate.change$last.time <- ymd_hms(lactate.change$last.time)

for (i in 1:length(lactates.id)) {
  if (lactates.2[i, 4] <= 0.9 * lactates.2[i, 10]) {
    decrease10 <- 'Y'
  }
  else {decrease10 <- 'N'}
  
  if (lactates.2[i, 4] <= 2.5) {
    normalization <- 'Y'
  }
  else {normalization <- 'N'}  
  
  lactate.change$first.time[i] <- lactates.2[i, 8]
  lactate.change$last.time[i] <- lactates.2[i, 2]
  lactate.change$first.valuenum[i] <- lactates.2[i, 10]
  lactate.change$last.valuenum[i] <- lactates.2[i, 4]
  lactate.change$decrease_10[i] <- decrease10
  lactate.change$normalized[i] <- normalization
}


lactate.decrease10 <- lactate.change[lactate.change$decrease_10 == 'Y',]
lactate.non_decrease10 <- lactate.change[lactate.change$decrease_10 == 'N',]
lactate.normalization <- lactate.change[lactate.change$normalized == 'Y',]
lactate.non_normalization <- lactate.change[lactate.change$normalized == 'N',]


lactate.decrease10.id <- unique(lactate.decrease10$id)
lactate.non_decrease10.id <- unique(lactate.non_decrease10$id) 
lactate.normalization.id <- unique(lactate.normalization$id) 
lactate.non_normalization.id <- unique(lactate.non_normalization$id)


lactate.decrease10.id2 <- hospids(lactate.decrease10.id) 
lactate.non_decrease10.id2 <- hospids(lactate.non_decrease10.id) 
lactate.normalization.id2 <- hospids(lactate.normalization.id) 
lactate.non_normalization.id2 <- hospids(lactate.non_normalization.id) 


lactate.decrease10.hosp_mortality <- intersect(lactate.decrease10.id, hosp_mortality.id) 
lactate.non_decrease10.hosp_mortality <- intersect(lactate.non_decrease10.id, hosp_mortality.id)
lactate.normalization.hosp_mortality <- intersect(lactate.normalization.id, hosp_mortality.id) 
lactate.non_normalization.hosp_mortality <- intersect(lactate.non_normalization.id, hosp_mortality.id) 


lactate.decrease10.icu_mortality <- intersect(lactate.decrease10.id, icu_mortality.id) 
lactate.non_decrease10.icu_mortality <- intersect(lactate.non_decrease10.id, icu_mortality.id) 
lactate.normalization.icu_mortality <- intersect(lactate.normalization.id, icu_mortality.id) 
lactate.non_normalization.icu_mortality <- intersect(lactate.non_normalization.id, icu_mortality.id) 


lactate.decrease10.mortality30 <- intersect(lactate.decrease10.id, df1.mortality30.id)
lactate.non_decrease10.mortality30 <- intersect(lactate.non_decrease10.id, df1.mortality30.id) 
lactate.normalization.mortality30 <- intersect(lactate.normalization.id, df1.mortality30.id) 
lactate.non_normalization.mortality30 <- intersect(lactate.non_normalization.id, df1.mortality30.id) 


length(lactate.decrease10.icu_mortality) / length(lactate.decrease10.id)
length(lactate.decrease10.hosp_mortality) / length(lactate.decrease10.id2)
length(lactate.decrease10.mortality30) / length(lactate.decrease10.id)


length(lactate.non_decrease10.icu_mortality) / length(lactate.non_decrease10.id)
length(lactate.non_decrease10.hosp_mortality) / length(lactate.non_decrease10.id2)
length(lactate.non_decrease10.mortality30) / length(lactate.non_decrease10.id)


length(lactate.normalization.icu_mortality) / length(lactate.normalization.id)
length(lactate.normalization.hosp_mortality) / length(lactate.normalization.id2)
length(lactate.normalization.mortality30) / length(lactate.normalization.id)


length(lactate.non_normalization.icu_mortality) / length(lactate.non_normalization.id)
length(lactate.non_normalization.hosp_mortality) / length(lactate.non_normalization.id2)
length(lactate.non_normalization.mortality30) / length(lactate.non_normalization.id)





#====================lactate change from time of event to the first 24 hours========================================
#get the lactate test by every 6 hours
lactate.first24 <- data.frame(matrix(ncol = 0, nrow = 0))
f.lactate.first24 <- function(dataset){
  # Identify time of event
  first24 <- dataset[(abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 24) == min(abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 24))) & (abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 24) <= 2), ][1,]
  lactate.first24  <- rbind(lactate.first24, first24)
}

lactate.first24 <- ddply(lactates, .(id), f.lactate.first24) 
lactate.first24$initial.lactate <- lactates.first0$valuenum
lactate.first24 <- lactate.first24[!is.na(lactate.first24$id), ]

lactates.2 <- lactate.first24
head(lactates.2)

#for each patient,analyze whether lactate level decrease by 10% and lactate normalized
lactates.id <- unique(lactates.2$id)
lactate.change <- data.frame(matrix(ncol = 7, nrow = length(lactates.id)))
names(lactate.change) <- c('id', 'first.time', 'first.valuenum','last.time', 'last.valuenum', 'decrease_10', 'normalized')
lactate.change$id <- lactates.id
lactate.change$first.time <- ymd_hms(lactate.change$first.time)
lactate.change$last.time <- ymd_hms(lactate.change$last.time)

for (i in 1:length(lactates.id)) {
  if (lactates.2[i, 4] <= 0.9 * lactates.2[i, 10]) {
    decrease10 <- 'Y'
  }
  else {decrease10 <- 'N'}
  
  if (lactates.2[i, 4] <= 2.5) {
    normalization <- 'Y'
  }
  else {normalization <- 'N'}  
  
  lactate.change$first.time[i] <- lactates.2[i, 8]
  lactate.change$last.time[i] <- lactates.2[i, 2]
  lactate.change$first.valuenum[i] <- lactates.2[i, 10]
  lactate.change$last.valuenum[i] <- lactates.2[i, 4]
  lactate.change$decrease_10[i] <- decrease10
  lactate.change$normalized[i] <- normalization
}


lactate.decrease10 <- lactate.change[lactate.change$decrease_10 == 'Y',]
lactate.non_decrease10 <- lactate.change[lactate.change$decrease_10 == 'N',]
lactate.normalization <- lactate.change[lactate.change$normalized == 'Y',]
lactate.non_normalization <- lactate.change[lactate.change$normalized == 'N',]


lactate.decrease10.id <- unique(lactate.decrease10$id)
lactate.non_decrease10.id <- unique(lactate.non_decrease10$id) 
lactate.normalization.id <- unique(lactate.normalization$id) 
lactate.non_normalization.id <- unique(lactate.non_normalization$id)


lactate.decrease10.id2 <- hospids(lactate.decrease10.id) 
lactate.non_decrease10.id2 <- hospids(lactate.non_decrease10.id) 
lactate.normalization.id2 <- hospids(lactate.normalization.id) 
lactate.non_normalization.id2 <- hospids(lactate.non_normalization.id) 


lactate.decrease10.hosp_mortality <- intersect(lactate.decrease10.id, hosp_mortality.id) 
lactate.non_decrease10.hosp_mortality <- intersect(lactate.non_decrease10.id, hosp_mortality.id)
lactate.normalization.hosp_mortality <- intersect(lactate.normalization.id, hosp_mortality.id) 
lactate.non_normalization.hosp_mortality <- intersect(lactate.non_normalization.id, hosp_mortality.id) 


lactate.decrease10.icu_mortality <- intersect(lactate.decrease10.id, icu_mortality.id) 
lactate.non_decrease10.icu_mortality <- intersect(lactate.non_decrease10.id, icu_mortality.id) 
lactate.normalization.icu_mortality <- intersect(lactate.normalization.id, icu_mortality.id) 
lactate.non_normalization.icu_mortality <- intersect(lactate.non_normalization.id, icu_mortality.id) 


lactate.decrease10.mortality30 <- intersect(lactate.decrease10.id, df1.mortality30.id)
lactate.non_decrease10.mortality30 <- intersect(lactate.non_decrease10.id, df1.mortality30.id) 
lactate.normalization.mortality30 <- intersect(lactate.normalization.id, df1.mortality30.id) 
lactate.non_normalization.mortality30 <- intersect(lactate.non_normalization.id, df1.mortality30.id) 


length(lactate.decrease10.icu_mortality) / length(lactate.decrease10.id)
length(lactate.decrease10.hosp_mortality) / length(lactate.decrease10.id2)
length(lactate.decrease10.mortality30) / length(lactate.decrease10.id)


length(lactate.non_decrease10.icu_mortality) / length(lactate.non_decrease10.id)
length(lactate.non_decrease10.hosp_mortality) / length(lactate.non_decrease10.id2)
length(lactate.non_decrease10.mortality30) / length(lactate.non_decrease10.id)


length(lactate.normalization.icu_mortality) / length(lactate.normalization.id)
length(lactate.normalization.hosp_mortality) / length(lactate.normalization.id2)
length(lactate.normalization.mortality30) / length(lactate.normalization.id)


length(lactate.non_normalization.icu_mortality) / length(lactate.non_normalization.id)
length(lactate.non_normalization.hosp_mortality) / length(lactate.non_normalization.id2)
length(lactate.non_normalization.mortality30) / length(lactate.non_normalization.id)



#===================================lactate changes for sepsis patients under definition 6================================
# severe sepsis patients with lactate > 2.5 by definition 6
load(file = 'severe.sepsis2.all.2.RData')
severe.sepsis.all.2 <- severe.sepsis2.all.2[order(severe.sepsis2.all.2$id, severe.sepsis2.all.2$charttime),]
head(severe.sepsis.all.2)

#remove those items before time of event (severe sepsis) and only keep those after developed severe sepsis
severe.sepsis.all.3 <- severe.sepsis.all.2[difftime(severe.sepsis.all.2$charttime, severe.sepsis.all.2$time.event, units = 'hours') >= 0, ]
lactates <- severe.sepsis.all.3[(severe.sepsis.all.3$itemid == 50010 | severe.sepsis.all.3$itemid == 818 | severe.sepsis.all.3$itemid == 1531),]
head(lactates)
lactates <- lactates[!is.na(lactates$valuenum),]
lactates$lactate.timediff <- difftime(lactates$charttime, lactates$time.event, units = 'hours')

#get the first lactate test and analyze the time of the first lactate test from severe sepsis
lactate.first <- data.frame(matrix(ncol = 0, nrow = 0))
f.lactate.begin <- function(dataset){
  # Identify time of event
  first <- dataset[1,]
  lactate.first  <- rbind(lactate.first, first)
  return(lactate.first)
}
#all the first lactate is the time of event
lactates.first0 <- ddply(lactates, .(id), f.lactate.begin) 

#get the last lactate test and analyze the time of the last lactate test from severe sepsis
lactate.last <- data.frame(matrix(ncol = 0, nrow = 0))
f.lactate.end <- function(dataset){
  # Identify time of event
  len <- dim(dataset)[1]
  last <- dataset[len,]
  lactate.last  <- rbind(lactate.last, last)
  return(lactate.last)
}
#all the first lactate is the time of event
lactates.last0 <- ddply(lactates, .(id), f.lactate.end) 

d <- density(as.numeric(lactates.last0$lactate.timediff))
plot(d, main = 'Time of last lactate test from time of event')
boxplot(as.numeric(lactates.last0$lactate.timediff))
summary(as.numeric(lactates.last0$lactate.timediff))


lactates.last1 <- lactates.last0[order(lactates.last0$lactate.timediff),]
d <- density(as.numeric(lactates.last1$lactate.timediff[1:2000]))
plot(d, main = 'Time of last lactate test from time of event[1:2000]')
boxplot(as.numeric(lactates.last1$lactate.timediff[1:2000]))
summary(as.numeric(lactates.last1$lactate.timediff[1:1200]))


#====================lactate change from time of event to the first 6 hours========================================
#get the lactate test by every 6 hours
lactate.first6 <- data.frame(matrix(ncol = 0, nrow = 0))
f.lactate.first6 <- function(dataset){
  # Identify time of event
  first6 <- dataset[(abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 6) == min(abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 6))) & (abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 6) <= 2), ][1,]
  lactate.first6  <- rbind(lactate.first6, first6)
}

lactate.first6 <- ddply(lactates, .(id), f.lactate.first6) 
lactate.first6$initial.lactate <- lactates.first0$valuenum
lactate.first6 <- lactate.first6[!is.na(lactate.first6$id), ]

lactates.2 <- lactate.first6
head(lactates.2)

#for each patient,analyze whether lactate level decrease by 10% and lactate normalized
lactates.id <- unique(lactates.2$id)
lactate.change <- data.frame(matrix(ncol = 7, nrow = length(lactates.id)))
names(lactate.change) <- c('id', 'first.time', 'first.valuenum','last.time', 'last.valuenum', 'decrease_10', 'normalized')
lactate.change$id <- lactates.id
lactate.change$first.time <- ymd_hms(lactate.change$first.time)
lactate.change$last.time <- ymd_hms(lactate.change$last.time)

for (i in 1:length(lactates.id)) {
  if (lactates.2[i, 4] <= 0.9 * lactates.2[i, 10]) {
    decrease10 <- 'Y'
  }
  else {decrease10 <- 'N'}
  
  if (lactates.2[i, 4] <= 2.5) {
    normalization <- 'Y'
  }
  else {normalization <- 'N'}  
  
  lactate.change$first.time[i] <- lactates.2[i, 8]
  lactate.change$last.time[i] <- lactates.2[i, 2]
  lactate.change$first.valuenum[i] <- lactates.2[i, 10]
  lactate.change$last.valuenum[i] <- lactates.2[i, 4]
  lactate.change$decrease_10[i] <- decrease10
  lactate.change$normalized[i] <- normalization
}


lactate.decrease10 <- lactate.change[lactate.change$decrease_10 == 'Y',]
lactate.non_decrease10 <- lactate.change[lactate.change$decrease_10 == 'N',]
lactate.normalization <- lactate.change[lactate.change$normalized == 'Y',]
lactate.non_normalization <- lactate.change[lactate.change$normalized == 'N',]


lactate.decrease10.id <- unique(lactate.decrease10$id)
lactate.non_decrease10.id <- unique(lactate.non_decrease10$id) 
lactate.normalization.id <- unique(lactate.normalization$id) 
lactate.non_normalization.id <- unique(lactate.non_normalization$id)


lactate.decrease10.id2 <- hospids(lactate.decrease10.id) 
lactate.non_decrease10.id2 <- hospids(lactate.non_decrease10.id) 
lactate.normalization.id2 <- hospids(lactate.normalization.id) 
lactate.non_normalization.id2 <- hospids(lactate.non_normalization.id) 


lactate.decrease10.hosp_mortality <- intersect(lactate.decrease10.id, hosp_mortality.id) 
lactate.non_decrease10.hosp_mortality <- intersect(lactate.non_decrease10.id, hosp_mortality.id)
lactate.normalization.hosp_mortality <- intersect(lactate.normalization.id, hosp_mortality.id) 
lactate.non_normalization.hosp_mortality <- intersect(lactate.non_normalization.id, hosp_mortality.id) 


lactate.decrease10.icu_mortality <- intersect(lactate.decrease10.id, icu_mortality.id) 
lactate.non_decrease10.icu_mortality <- intersect(lactate.non_decrease10.id, icu_mortality.id) 
lactate.normalization.icu_mortality <- intersect(lactate.normalization.id, icu_mortality.id) 
lactate.non_normalization.icu_mortality <- intersect(lactate.non_normalization.id, icu_mortality.id) 


lactate.decrease10.mortality30 <- intersect(lactate.decrease10.id, df6.mortality30.id)
lactate.non_decrease10.mortality30 <- intersect(lactate.non_decrease10.id, df6.mortality30.id) 
lactate.normalization.mortality30 <- intersect(lactate.normalization.id, df6.mortality30.id) 
lactate.non_normalization.mortality30 <- intersect(lactate.non_normalization.id, df6.mortality30.id) 


length(lactate.decrease10.icu_mortality) / length(lactate.decrease10.id)
length(lactate.decrease10.hosp_mortality) / length(lactate.decrease10.id2)
length(lactate.decrease10.mortality30) / length(lactate.decrease10.id)


length(lactate.non_decrease10.icu_mortality) / length(lactate.non_decrease10.id)
length(lactate.non_decrease10.hosp_mortality) / length(lactate.non_decrease10.id2)
length(lactate.non_decrease10.mortality30) / length(lactate.non_decrease10.id)

length(lactate.normalization.icu_mortality) / length(lactate.normalization.id)
length(lactate.normalization.hosp_mortality) / length(lactate.normalization.id2)
length(lactate.normalization.mortality30) / length(lactate.normalization.id)


length(lactate.non_normalization.icu_mortality) / length(lactate.non_normalization.id)
length(lactate.non_normalization.hosp_mortality) / length(lactate.non_normalization.id2)
length(lactate.non_normalization.mortality30) / length(lactate.non_normalization.id)



#====================lactate change from time of event to the first 12 hours========================================
#get the lactate test by every 6 hours
lactate.first12 <- data.frame(matrix(ncol = 0, nrow = 0))
f.lactate.first12 <- function(dataset){
  # Identify time of event
  first12 <- dataset[(abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 12) == min(abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 12))) & (abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 12) <= 2), ][1,]
  lactate.first12  <- rbind(lactate.first12, first12)
}

lactate.first12 <- ddply(lactates, .(id), f.lactate.first12) 
lactate.first12$initial.lactate <- lactates.first0$valuenum
lactate.first12 <- lactate.first12[!is.na(lactate.first12$id), ]

lactates.2 <- lactate.first12
head(lactates.2)

#for each patient,analyze whether lactate level decrease by 10% and lactate normalized
lactates.id <- unique(lactates.2$id)
lactate.change <- data.frame(matrix(ncol = 7, nrow = length(lactates.id)))
names(lactate.change) <- c('id', 'first.time', 'first.valuenum','last.time', 'last.valuenum', 'decrease_10', 'normalized')
lactate.change$id <- lactates.id
lactate.change$first.time <- ymd_hms(lactate.change$first.time)
lactate.change$last.time <- ymd_hms(lactate.change$last.time)

for (i in 1:length(lactates.id)) {
  if (lactates.2[i, 4] <= 0.9 * lactates.2[i, 10]) {
    decrease10 <- 'Y'
  }
  else {decrease10 <- 'N'}
  
  if (lactates.2[i, 4] <= 2.5) {
    normalization <- 'Y'
  }
  else {normalization <- 'N'}  
  
  lactate.change$first.time[i] <- lactates.2[i, 8]
  lactate.change$last.time[i] <- lactates.2[i, 2]
  lactate.change$first.valuenum[i] <- lactates.2[i, 10]
  lactate.change$last.valuenum[i] <- lactates.2[i, 4]
  lactate.change$decrease_10[i] <- decrease10
  lactate.change$normalized[i] <- normalization
}


lactate.decrease10 <- lactate.change[lactate.change$decrease_10 == 'Y',]
lactate.non_decrease10 <- lactate.change[lactate.change$decrease_10 == 'N',]
lactate.normalization <- lactate.change[lactate.change$normalized == 'Y',]
lactate.non_normalization <- lactate.change[lactate.change$normalized == 'N',]


lactate.decrease10.id <- unique(lactate.decrease10$id)
lactate.non_decrease10.id <- unique(lactate.non_decrease10$id) 
lactate.normalization.id <- unique(lactate.normalization$id) 
lactate.non_normalization.id <- unique(lactate.non_normalization$id)


lactate.decrease10.id2 <- hospids(lactate.decrease10.id) 
lactate.non_decrease10.id2 <- hospids(lactate.non_decrease10.id) 
lactate.normalization.id2 <- hospids(lactate.normalization.id) 
lactate.non_normalization.id2 <- hospids(lactate.non_normalization.id) 


lactate.decrease10.hosp_mortality <- intersect(lactate.decrease10.id, hosp_mortality.id) 
lactate.non_decrease10.hosp_mortality <- intersect(lactate.non_decrease10.id, hosp_mortality.id)
lactate.normalization.hosp_mortality <- intersect(lactate.normalization.id, hosp_mortality.id) 
lactate.non_normalization.hosp_mortality <- intersect(lactate.non_normalization.id, hosp_mortality.id) 


lactate.decrease10.icu_mortality <- intersect(lactate.decrease10.id, icu_mortality.id) 
lactate.non_decrease10.icu_mortality <- intersect(lactate.non_decrease10.id, icu_mortality.id) 
lactate.normalization.icu_mortality <- intersect(lactate.normalization.id, icu_mortality.id) 
lactate.non_normalization.icu_mortality <- intersect(lactate.non_normalization.id, icu_mortality.id) 


lactate.decrease10.mortality30 <- intersect(lactate.decrease10.id, df6.mortality30.id)
lactate.non_decrease10.mortality30 <- intersect(lactate.non_decrease10.id, df6.mortality30.id) 
lactate.normalization.mortality30 <- intersect(lactate.normalization.id, df6.mortality30.id) 
lactate.non_normalization.mortality30 <- intersect(lactate.non_normalization.id, df6.mortality30.id) 


length(lactate.decrease10.icu_mortality) / length(lactate.decrease10.id)
length(lactate.decrease10.hosp_mortality) / length(lactate.decrease10.id2)
length(lactate.decrease10.mortality30) / length(lactate.decrease10.id)


length(lactate.non_decrease10.icu_mortality) / length(lactate.non_decrease10.id)
length(lactate.non_decrease10.hosp_mortality) / length(lactate.non_decrease10.id2)
length(lactate.non_decrease10.mortality30) / length(lactate.non_decrease10.id)

length(lactate.normalization.icu_mortality) / length(lactate.normalization.id)
length(lactate.normalization.hosp_mortality) / length(lactate.normalization.id2)
length(lactate.normalization.mortality30) / length(lactate.normalization.id)


length(lactate.non_normalization.icu_mortality) / length(lactate.non_normalization.id)
length(lactate.non_normalization.hosp_mortality) / length(lactate.non_normalization.id2)
length(lactate.non_normalization.mortality30) / length(lactate.non_normalization.id)



#====================lactate change from time of event to the first 18 hours========================================
#get the lactate test by every 6 hours
lactate.first18 <- data.frame(matrix(ncol = 0, nrow = 0))
f.lactate.first18 <- function(dataset){
  # Identify time of event
  first18 <- dataset[(abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 18) == min(abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 18))) & (abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 18) <= 2), ][1,]
  lactate.first18  <- rbind(lactate.first18, first18)
}

lactate.first18 <- ddply(lactates, .(id), f.lactate.first18) 
lactate.first18$initial.lactate <- lactates.first0$valuenum
lactate.first18 <- lactate.first18[!is.na(lactate.first18$id), ]

lactates.2 <- lactate.first18
head(lactates.2)

#for each patient,analyze whether lactate level decrease by 10% and lactate normalized
lactates.id <- unique(lactates.2$id)
lactate.change <- data.frame(matrix(ncol = 7, nrow = length(lactates.id)))
names(lactate.change) <- c('id', 'first.time', 'first.valuenum','last.time', 'last.valuenum', 'decrease_10', 'normalized')
lactate.change$id <- lactates.id
lactate.change$first.time <- ymd_hms(lactate.change$first.time)
lactate.change$last.time <- ymd_hms(lactate.change$last.time)

for (i in 1:length(lactates.id)) {
  if (lactates.2[i, 4] <= 0.9 * lactates.2[i, 10]) {
    decrease10 <- 'Y'
  }
  else {decrease10 <- 'N'}
  
  if (lactates.2[i, 4] <= 2.5) {
    normalization <- 'Y'
  }
  else {normalization <- 'N'}  
  
  lactate.change$first.time[i] <- lactates.2[i, 8]
  lactate.change$last.time[i] <- lactates.2[i, 2]
  lactate.change$first.valuenum[i] <- lactates.2[i, 10]
  lactate.change$last.valuenum[i] <- lactates.2[i, 4]
  lactate.change$decrease_10[i] <- decrease10
  lactate.change$normalized[i] <- normalization
}


lactate.decrease10 <- lactate.change[lactate.change$decrease_10 == 'Y',]
lactate.non_decrease10 <- lactate.change[lactate.change$decrease_10 == 'N',]
lactate.normalization <- lactate.change[lactate.change$normalized == 'Y',]
lactate.non_normalization <- lactate.change[lactate.change$normalized == 'N',]


lactate.decrease10.id <- unique(lactate.decrease10$id)
lactate.non_decrease10.id <- unique(lactate.non_decrease10$id) 
lactate.normalization.id <- unique(lactate.normalization$id) 
lactate.non_normalization.id <- unique(lactate.non_normalization$id)


lactate.decrease10.id2 <- hospids(lactate.decrease10.id) 
lactate.non_decrease10.id2 <- hospids(lactate.non_decrease10.id) 
lactate.normalization.id2 <- hospids(lactate.normalization.id) 
lactate.non_normalization.id2 <- hospids(lactate.non_normalization.id) 


lactate.decrease10.hosp_mortality <- intersect(lactate.decrease10.id, hosp_mortality.id) 
lactate.non_decrease10.hosp_mortality <- intersect(lactate.non_decrease10.id, hosp_mortality.id)
lactate.normalization.hosp_mortality <- intersect(lactate.normalization.id, hosp_mortality.id) 
lactate.non_normalization.hosp_mortality <- intersect(lactate.non_normalization.id, hosp_mortality.id) 


lactate.decrease10.icu_mortality <- intersect(lactate.decrease10.id, icu_mortality.id) 
lactate.non_decrease10.icu_mortality <- intersect(lactate.non_decrease10.id, icu_mortality.id) 
lactate.normalization.icu_mortality <- intersect(lactate.normalization.id, icu_mortality.id) 
lactate.non_normalization.icu_mortality <- intersect(lactate.non_normalization.id, icu_mortality.id) 


lactate.decrease10.mortality30 <- intersect(lactate.decrease10.id, df6.mortality30.id)
lactate.non_decrease10.mortality30 <- intersect(lactate.non_decrease10.id, df6.mortality30.id) 
lactate.normalization.mortality30 <- intersect(lactate.normalization.id, df6.mortality30.id) 
lactate.non_normalization.mortality30 <- intersect(lactate.non_normalization.id, df6.mortality30.id) 


length(lactate.decrease10.icu_mortality) / length(lactate.decrease10.id)
length(lactate.decrease10.hosp_mortality) / length(lactate.decrease10.id2)
length(lactate.decrease10.mortality30) / length(lactate.decrease10.id)


length(lactate.non_decrease10.icu_mortality) / length(lactate.non_decrease10.id)
length(lactate.non_decrease10.hosp_mortality) / length(lactate.non_decrease10.id2)
length(lactate.non_decrease10.mortality30) / length(lactate.non_decrease10.id)


length(lactate.normalization.icu_mortality) / length(lactate.normalization.id)
length(lactate.normalization.hosp_mortality) / length(lactate.normalization.id2)
length(lactate.normalization.mortality30) / length(lactate.normalization.id)


length(lactate.non_normalization.icu_mortality) / length(lactate.non_normalization.id)
length(lactate.non_normalization.hosp_mortality) / length(lactate.non_normalization.id2)
length(lactate.non_normalization.mortality30) / length(lactate.non_normalization.id)





#====================lactate change from time of event to the first 24 hours========================================
#get the lactate test by every 6 hours
lactate.first24 <- data.frame(matrix(ncol = 0, nrow = 0))
f.lactate.first24 <- function(dataset){
  # Identify time of event
  first24 <- dataset[(abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 24) == min(abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 24))) & (abs(difftime(dataset$charttime, dataset$time.event, units = 'hours') - 24) <= 2), ][1,]
  lactate.first24  <- rbind(lactate.first24, first24)
}

lactate.first24 <- ddply(lactates, .(id), f.lactate.first24) 
lactate.first24$initial.lactate <- lactates.first0$valuenum
lactate.first24 <- lactate.first24[!is.na(lactate.first24$id), ]

lactates.2 <- lactate.first24
head(lactates.2)

#for each patient,analyze whether lactate level decrease by 10% and lactate normalized
lactates.id <- unique(lactates.2$id)
lactate.change <- data.frame(matrix(ncol = 7, nrow = length(lactates.id)))
names(lactate.change) <- c('id', 'first.time', 'first.valuenum','last.time', 'last.valuenum', 'decrease_10', 'normalized')
lactate.change$id <- lactates.id
lactate.change$first.time <- ymd_hms(lactate.change$first.time)
lactate.change$last.time <- ymd_hms(lactate.change$last.time)

for (i in 1:length(lactates.id)) {
  if (lactates.2[i, 4] <= 0.9 * lactates.2[i, 10]) {
    decrease10 <- 'Y'
  }
  else {decrease10 <- 'N'}
  
  if (lactates.2[i, 4] <= 2.5) {
    normalization <- 'Y'
  }
  else {normalization <- 'N'}  
  
  lactate.change$first.time[i] <- lactates.2[i, 8]
  lactate.change$last.time[i] <- lactates.2[i, 2]
  lactate.change$first.valuenum[i] <- lactates.2[i, 10]
  lactate.change$last.valuenum[i] <- lactates.2[i, 4]
  lactate.change$decrease_10[i] <- decrease10
  lactate.change$normalized[i] <- normalization
}


lactate.decrease10 <- lactate.change[lactate.change$decrease_10 == 'Y',]
lactate.non_decrease10 <- lactate.change[lactate.change$decrease_10 == 'N',]
lactate.normalization <- lactate.change[lactate.change$normalized == 'Y',]
lactate.non_normalization <- lactate.change[lactate.change$normalized == 'N',]


lactate.decrease10.id <- unique(lactate.decrease10$id)
lactate.non_decrease10.id <- unique(lactate.non_decrease10$id) 
lactate.normalization.id <- unique(lactate.normalization$id) 
lactate.non_normalization.id <- unique(lactate.non_normalization$id)


lactate.decrease10.id2 <- hospids(lactate.decrease10.id) 
lactate.non_decrease10.id2 <- hospids(lactate.non_decrease10.id) 
lactate.normalization.id2 <- hospids(lactate.normalization.id) 
lactate.non_normalization.id2 <- hospids(lactate.non_normalization.id) 


lactate.decrease10.hosp_mortality <- intersect(lactate.decrease10.id, hosp_mortality.id) 
lactate.non_decrease10.hosp_mortality <- intersect(lactate.non_decrease10.id, hosp_mortality.id)
lactate.normalization.hosp_mortality <- intersect(lactate.normalization.id, hosp_mortality.id) 
lactate.non_normalization.hosp_mortality <- intersect(lactate.non_normalization.id, hosp_mortality.id) 


lactate.decrease10.icu_mortality <- intersect(lactate.decrease10.id, icu_mortality.id) 
lactate.non_decrease10.icu_mortality <- intersect(lactate.non_decrease10.id, icu_mortality.id) 
lactate.normalization.icu_mortality <- intersect(lactate.normalization.id, icu_mortality.id) 
lactate.non_normalization.icu_mortality <- intersect(lactate.non_normalization.id, icu_mortality.id) 


lactate.decrease10.mortality30 <- intersect(lactate.decrease10.id, df6.mortality30.id)
lactate.non_decrease10.mortality30 <- intersect(lactate.non_decrease10.id, df6.mortality30.id) 
lactate.normalization.mortality30 <- intersect(lactate.normalization.id, df6.mortality30.id) 
lactate.non_normalization.mortality30 <- intersect(lactate.non_normalization.id, df6.mortality30.id) 


length(lactate.decrease10.icu_mortality) / length(lactate.decrease10.id)
length(lactate.decrease10.hosp_mortality) / length(lactate.decrease10.id2)
length(lactate.decrease10.mortality30) / length(lactate.decrease10.id)


length(lactate.non_decrease10.icu_mortality) / length(lactate.non_decrease10.id)
length(lactate.non_decrease10.hosp_mortality) / length(lactate.non_decrease10.id2)
length(lactate.non_decrease10.mortality30) / length(lactate.non_decrease10.id)


length(lactate.normalization.icu_mortality) / length(lactate.normalization.id)
length(lactate.normalization.hosp_mortality) / length(lactate.normalization.id2)
length(lactate.normalization.mortality30) / length(lactate.normalization.id)


length(lactate.non_normalization.icu_mortality) / length(lactate.non_normalization.id)
length(lactate.non_normalization.hosp_mortality) / length(lactate.non_normalization.id2)
length(lactate.non_normalization.mortality30) / length(lactate.non_normalization.id)


