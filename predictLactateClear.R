# predictive modeling on lactate clearance and normalization

library(lubridate)
library(plyr)
# install.packages('ggplot2')
library(ggplot2)
# to get the time of severe sepsis event
load("severe.sepsis2.eventtime.3.RData")
names(severe.sepsis2.eventtime.3)
sepsis.time <- data.frame(severe.sepsis2.eventtime.3$id, severe.sepsis2.eventtime.3$charttime)
names(sepsis.time) <- c('id', 'event.time')

# read severe.sepsis data which only contains those after the time of event (severe sepsis)
load("severe.sepsis.RData")
names(severe.sepsis)
unique(severe.sepsis$itemid)
# to get the rows of only tests of interest
variables.toremove <- c(50010, 818, 1531, 70011, 70012)
severe.sepsis.v <- severe.sepsis[!(severe.sepsis$itemid %in% variables.toremove), ]
# severe.sepsis.v <- severe.sepsis.v[c(50002, 50015, 50019, 50018, 50016, 50439, 50440, 50460, 50009, 50149, 50012, 50159), ]


# 
# severesepsis.sepsistime <- merge(severe.sepsis, sepsis.time, by.x = 'id', by.y = 'id', all.x = T)
# sepsis.ptid <- unique(severesepsis.sepsistime$id)
# head(severesepsis.sepsistime)
# str(severesepsis.sepsistime)
# severesepsis.afterevent <- severesepsis.sepsistime[(!is.na(severesepsis.sepsistime$charttime)) & (severesepsis.sepsistime$charttime >= severesepsis.sepsistime$event.time), ]

load('lactateclearance.RData')
severe.sepsis.v.clearance <- merge(severe.sepsis.v, lactateclearance, by.x = 'id', by.y = 'id', all.x = T)
head(severe.sepsis.v.clearance)
severe.sepsis.v.clearance.eventtime <- merge(severe.sepsis.v.clearance, sepsis.time, by.x = 'id', by.y = 'id', all.x = T)
severe.sepsis.v.clearance.eventtime[11500:11520,]
summary(severe.sepsis.v.clearance.eventtime$cleartime)


# get the row number of patients who cleared or censored in terms of their lacate level
selectrows.clear <- c()
selectrows.noclear <- c()
for (i in 1:nrow(severe.sepsis.v.clearance.eventtime)) {
  if (!is.na(severe.sepsis.v.clearance.eventtime$cleartime[i])) {
    if (difftime(severe.sepsis.v.clearance.eventtime$charttime[i], severe.sepsis.v.clearance.eventtime$event.time[i], units = 'hours') < severe.sepsis.v.clearance.eventtime$cleartime[i]) {
      selectrows.clear <- c(selectrows.clear, i)
    }    
  }
  else {
    if (difftime(severe.sepsis.v.clearance.eventtime$charttime[i], severe.sepsis.v.clearance.eventtime$event.time[i], units = 'hours') < severe.sepsis.v.clearance.eventtime$lastlactate.dftime[i]) {
      selectrows.noclear <- c(selectrows.noclear, i)
    }        
  }
}

# the data of patients who cleared their lactate 
data.cleared <- severe.sepsis.v.clearance.eventtime[selectrows.clear, ]
data.cleared$exit.time <-  data.cleared$cleartime
data.cleared$response <-  1
str(data.cleared)
# the data of patients who are censored in terms of the lactate
data.nocleared <- severe.sepsis.v.clearance.eventtime[selectrows.noclear, ]
data.nocleared$exit.time <- data.nocleared$lastlactate.dftime
data.nocleared$response <-  0
str(data.nocleared)

data <- rbind(data.cleared, data.nocleared)
names(data)

#get only the id, initial, lactate, exit.time, response of each patient
data.info <- unique(data.frame(data$id, data$initial.lactate, data$exit.time, data$response))
names(data.info) <- c('id', 'initial.lactate', 'exit.time', 'response')


#get the last three values of each test of each patient
lasttests <- function(dataset) {
  if (nrow(dataset) >= 3) {
    v1 <- dataset$valuenum[(nrow(dataset)-2)]
    v2 <- dataset$valuenum[(nrow(dataset)-1)]
    v3 <- dataset$valuenum[nrow(dataset)]
    v21 <- v2 - v1
    v32 <- v3 - v2
  }
  else if (nrow(dataset) == 2) {
    v1 <- NA
    v2 <- dataset$valuenum[(nrow(dataset)-1)]
    v3 <- dataset$valuenum[nrow(dataset)]
    v21 <- NA
    v32 <- v3 - v2    
  }
  else if (nrow(dataset) == 1) {
    v1 <- NA
    v2 <- NA
    v3 <- dataset$valuenum[nrow(dataset)]
    v21 <- NA
    v32 <- NA  
  }
  else {
    v1 <- NA
    v2 <- NA
    v3 <- NA
    v21 <- NA
    v32 <- NA  
  }
  variables <- c(v1, v2, v3, v21, v32)
  return(variables)
}

data <- data[order(data$id, data$charttime),]
data.2 <- data[data$itemid %in% c(198, 678, 211, 618, 455, 4552, 50002, 50015, 50019, 50018, 50016, 50439, 50440, 50460, 50009, 50149, 50012, 50159),] #variables used in their models (DSI-1 students)
data.2$itemid2 <- data.2$itemid
data.2$itemid2[data.2$itemid == 50440] <- 50460
data.2$itemid2[data.2$itemid == 50009] <- 50149
data.2$itemid2[data.2$itemid == 50012] <- 50159
# select the last three values of each test for each patients
data.last3 <- ddply(data.2, .(id, itemid2), lasttests)


flat.test.variables <- function(dataset) {
  variables.3 <- c()
  testids <- c(198, 678, 211, 618, 455, 4552, 50002, 50015, 50019, 50018, 50016, 50439, 50460, 50149, 50159)
  
  for (i in 1:length(testids)) {
    testrow <- dataset[dataset$itemid2 == testids[i],]
    if (nrow(testrow) == 1) {
      for (j in 3:7) {
        variables.2 <- testrow[1,j]
        variables.3 <- c(variables.3, variables.2)
      }      
    }
    else {
      variables.3 <- c(variables.3, rep(NA, 5))
    }
  }
  return(variables.3)
}

testids <- c(198, 678, 211, 618, 455, 4552, 50002, 50015, 50019, 50018, 50016, 50439, 50460, 50149, 50159)
testnames <- c() 
for (i in 1:length(testids)) {
  nm1 <- paste(testids[i], 'v1', sep = '_')
  nm2 <- paste(testids[i], 'v2', sep = '_')
  nm3 <- paste(testids[i], 'v3', sep = '_')
  nm21 <- paste(testids[i], 'v21', sep = '_')
  nm32 <- paste(testids[i], 'v32', sep = '_')
  testnames <- c(testnames, nm1, nm2, nm3, nm21, nm32)
}


data.last3.flat <- ddply(data.last3, .(id), flat.test.variables)
names(data.last3.flat) <- c('id', testnames)


data.3 <- merge(data.last3.flat, data.info, by.x = 'id', by.y = 'id', all.x = T)

# get the other infos of the patients including age, sex, first_SAPSI, first_SOFA, number of comorbidity
ptinfos3 <- read.csv('pticu_infos3.csv', header = T)
ptinfos3$id <- paste(ptinfos3$subject_id, ptinfos3$hospital_seq, ptinfos3$icustay_seq, sep='#%#')
names(ptinfos3)

pt.infos <- data.frame(ptinfos3$id, ptinfos3$gender, ptinfos3$icustay_admit_age, ptinfos3$sapsi_first, ptinfos3$sofa_first)
names(pt.infos) <- c('id', 'gender', 'age', 'sapsi_first', 'sofa_first')

data.4 <- merge(data.3, pt.infos, by.x = 'id', by.y = 'id', all.x = T)

# patients comorbidity info
comorbids <- read.csv('pts_comorbids_new.csv', header = T)
names(comorbids)
comorbids$score <- apply(comorbids[,-c(1,2,3)], 1, sum)
comorbids$id2 <- paste(comorbids$subject_id, comorbids$hospital_seq, sep='#%#')
comorbids2 <- data.frame(comorbids$id2, comorbids$score)
names(comorbids2) <- c('id2', 'score')


# install.packages("stringr")
library(stringr)
hospids <- function(ids) {
  ids2 <- lapply(ids, function (x) str_sub(x, 1, -5))
  ids2 <- unlist(ids2)
  return(ids2)
}

data.4$id2 <- hospids(data.4$id)

data.5 <- merge(data.4, comorbids2, by.x = 'id2', by.y = 'id2', all.x = T)
data.5$id2 <- NULL

names(data.5)


# Drop features that have less than 50% of data
num.na <- apply(data.5,2,function(x){return(length(which(is.na(x))))})
data.6 <- data.5[,num.na<nrow(data.5)/2]
names(data.6)


# Drop observations that have less than 50% of data
obs.na <- apply(data.6,1,function(x){return(length(which(is.na(x))))})
data.7 <- data.6[obs.na<(ncol(data.6)-3)/2,]
names(data.7)

#=========================data imputation===============================
# install.packages('DMwR')
library(DMwR)
table(data.7$response)
names(data.7)
data.7$id <- as.character(data.7$id)
data.8 <- knnImputation(data.7[,-c(1,70,71)], k=50, scale=T, meth="weighAvg")

data.8$id <- data.7$id
data.8$exit.time <- data.7$exit.time
data.8$response <- data.7$response
#save the data for further preditive modeing on lactate clearance
save(data.8, file = 'data.8.RData')
write.csv(data.8, file = 'data_lactate_clearance_predicton_df1.csv', row.names = F)



#========================================Predictive modeling on lactate clearance=======================================
load('data.8.RData')



#===================Cox Proportional Hazards Model======================================
library(survival)
names(data.8)
coxph_lactate.clear <- coxph(Surv(exit.time, response) ~ ., method = 'efron', data = data.8[,-1])
summary(coxph_lactate.clear)

# #===================logistic regression Model======================================
# data.6$response <- as.factor(data.6$response)
# lr_lactate.clear <- glm(response ~ ., data = data.6[,-c(1, 70)], family = 'binomial')
# summary(lr_lactate.clear)


library(cvTools)
set.seed(12345)
cvs <- cvFolds(nrow(data.6), K = 5, type = 'random')
for (i in 1:nfold) {
  testindex <- cvs$subsets[cvs$which==i]
  data.test <- data.6[testindex,]
  data.train <- data.6[-testindex,]
  
}
