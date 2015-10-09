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
  if (!is.na(severe.sepsis.v.clearance.eventtime$normalizetime[i])) {
    if (difftime(severe.sepsis.v.clearance.eventtime$charttime[i], severe.sepsis.v.clearance.eventtime$event.time[i], units = 'hours') < severe.sepsis.v.clearance.eventtime$normalizetime[i]) {
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

data.cleared$exit.time <-  data.cleared$normalizetime
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
summary(data.7)
data.7$id <- as.character(data.7$id)
data.8 <- knnImputation(data.7[,-c(1,76,77)], k=50, scale=T, meth="weighAvg")

data.8$id <- data.7$id
data.8$exit.time <- data.7$exit.time
data.8$response <- data.7$response
#save the data for further preditive modeing on lactate clearance
save(data.8, file = 'data.8.normalize.RData')
write.csv(data.8, file = 'data_lactate_normalize_predicton_df1.csv', row.names = F)



#========================================Predictive modeling on lactate clearance=======================================
load('data.8.normalize.RData')

#=====Cox Proportional Hazards Model======
library(survival)

names(data.8)
summary(data.8[data.8$response==0,])
stds0 <- apply(data.8[data.8$response==0,-c(75, 80, 81, 82)], 2, sd)

summary(data.8[data.8$response==1,])
stds1 <- apply(data.8[data.8$response==1,-c(75, 80, 81, 82)], 2, sd)


source("../R Codes/SPM_Panel-1.R")
uva.pairs("  ~  `198_v3` + `678_v3` + `211_v3` + `618_v3` + `455_v3` + `4552_v3` + `50002_v3` + `50015_v3` + exit.time", main = "Scatter Plot Matrix for Lactate Clearance", data = data.8,  labels = c('GCS', 'Temp', 'Pulse', 'Resp.', 'Systolic BP', 'Diastolic BP', 'Base Excess', 'Oxygen Saturation', 'exit.time'))

uva.pairs(" ~  `50019_v3` + `50018_v3` + `50016_v3` + `50439_v3` + `50460_v3` + `50149_v3` + `50159_v3` + exit.time", main = "Scatter Plot Matrix for Lactate Clearance", data = data.8,  labels = c('PO_2', 'pH Arterial', 'PCO_2', 'Protime', 'Partial Thromboplastin Time', 'Potassium', 'Sodium', 'exit.time'))

uva.pairs(" ~  `age` + `initial.lactate` + `score` + `sapsi_first` + `sofa_first` + exit.time", main = "Scatter Plot Matrix for Lactate Clearance", data = data.8,  labels = c('age', 'initial.lactate', 'score', 'sapsi_first', 'spfa_first', 'exit.time'))



par(mfrow=c(2,2))
boxplot(data.8[data.8$`198_v3`])
boxplot(data.8$exit.time)


data.8 <- data.8[data.8$gender != '',]
str(data.8)

boxplot(data.8$age)
summary(data.8$age)
 
boxplot(data.8$`678_v1`)
boxplot(data.8$`678_v2`)
boxplot(data.8$`678_v3`)

data.8.1 <- data.8[(data.8$exit.time < 10000), ]
boxplot(data.8.1$exit.time)
# select the patients with cleared lactate
data.8.2 <- data.8[(data.8$exit.time < 1000) & (data.8$age < 200) & (data.8$`678_v1` >50) & (data.8$`678_v2` >50) & (data.8$`678_v3` >50),]

boxplot(data.8.2$exit.time)
boxplot(data.8.2$age)
summary(data.8.2$age)

boxplot(data.8.2$`678_v1`)
boxplot(data.8.2$`678_v2`)
boxplot(data.8.2$`678_v3`)
#remove the outlier with extremely large exit.time
data.8.3 <- data.8.2[data.8.2$response == 1,]
boxplot(data.8.3$exit.time)

source("../R Codes/SPM_Panel-1.R")
uva.pairs("  ~  `198_v3` + `678_v3` + `211_v3` + `618_v3` + `455_v3` + `4552_v3` + `50002_v3` + `50015_v3` + exit.time", main = "Scatter Plot Matrix for Lactate Clearance", data = data.8.3,  labels = c('GCS', 'Temp', 'Pulse', 'Resp.', 'Systolic BP', 'Diastolic BP', 'Base Excess', 'Oxygen Saturation', 'exit.time'))

uva.pairs(" ~  `50019_v3` + `50018_v3` + `50016_v3` + `50439_v3` + `50460_v3` + `50149_v3` + `50159_v3` + exit.time", main = "Scatter Plot Matrix for Lactate Clearance", data = data.8.3,  labels = c('PO_2', 'pH Arterial', 'PCO_2', 'Protime', 'Partial Thromboplastin Time', 'Potassium', 'Sodium', 'exit.time'))

uva.pairs(" ~  `age` + `initial.lactate` + `score` + `sapsi_first` + `sofa_first` + exit.time", main = "Scatter Plot Matrix for Lactate Clearance", data = data.8.3,  labels = c('age', 'initial.lactate', 'score', 'sapsi_first', 'spfa_first', 'exit.time'))

# install.packages('caret')
# center and scale the independent variables
# library(caret)
# names(data.8.2)
# preProcValues <- preProcess(data.8.2[, -c(69, 74:76)], method=c("center", "scale"))
# data.8.4 <- predict(preProcValues, data.8.2[, -c(69, 74:76)])
# data.8.4 <- cbind(data.8.4, data.8.2[, c(69, 74:76)])
# data.8.5 <- data.8.4[data.8.4$response == 1,]
# 
# uva.pairs("  ~  `198_v3` + `678_v3` + `211_v3` + `618_v3` + `455_v3` + `4552_v3` + `50002_v3` + `50015_v3` + exit.time", main = "Scatter Plot Matrix for Lactate Clearance", data = data.8.5,  labels = c('GCS', 'Temp', 'Pulse', 'Resp.', 'Systolic BP', 'Diastolic BP', 'Base Excess', 'Oxygen Saturation', 'exit.time'))
# 
# uva.pairs(" ~  `50019_v3` + `50018_v3` + `50016_v3` + `50439_v3` + `50460_v3` + `50149_v3` + `50159_v3` + exit.time", main = "Scatter Plot Matrix for Lactate Clearance", data = data.8.5,  labels = c('PO_2', 'pH Arterial', 'PCO_2', 'Protime', 'Partial Thromboplastin Time', 'Potassium', 'Sodium', 'exit.time'))
# 
# uva.pairs(" ~  `age` + `initial.lactate` + `score` + `sapsi_first` + `sofa_first` + exit.time", main = "Scatter Plot Matrix for Lactate Clearance", data = data.8.5,  labels = c('age', 'initial.lactate', 'score', 'sapsi_first', 'spfa_first', 'exit.time'))




# data.9 <- data.8.2[,-74]
load('abtimes.RData')
# select the patients who had never had abx in the entire stay
abtimes.noabx <- abtimes[is.na(abtimes$first.abtime), ]


load('pts.firstabx.normalize.RData')
names(pts.firstabx)
# patients with antibiotics the time of antibiotics
data.9A <- merge(data.8.2, pts.firstabx, by.x = 'id', by.y= 'id') 
# patients without abx
data.9B <- data.8.2[data.8.2$id %in% abtimes.noabx$id, ] 

# coxph_lactate.clear <- coxph(Surv(exit.time, response) ~ ., method = 'efron', data = data.9[,-74])
# summary(coxph_lactate.clear)
# tab <- summary(coxph_lactate.clear)
# # install.packages('stargazer')
# library(stargazer)
# exp.coef <- exp(coxph_lactate.clear$coef)
# CI.vector <- exp(confint(coxph_lactate.clear))
# stargazer(coxph_lactate.clear, coef=list(exp.coef), single.row=T, ci=T, ci.custom=list(CI.vector))
# 
# boxplot(data.9$`50018_v32`)

# time.dep.zph <- cox.zph(coxph_lactate.clear, transform = 'log')
# time.dep.zph
# plot(time.dep.zph[3])
# abline(h=0, lty=3)
# plot(time.dep.zph[5])
#=====logistic regression Model===========
# data.6$response <- as.factor(data.6$response)
# lr_lactate.clear <- glm(response ~ ., data = data.6[,-c(1, 70)], family = 'binomial')
# summary(lr_lactate.clear)


#=========cross validation================
# library(cvTools)
# set.seed(12345)
# cvs <- cvFolds(nrow(data.6), K = 5, type = 'random')
# for (i in 1:nfold) {
#   testindex <- cvs$subsets[cvs$which==i]
#   data.test <- data.6[testindex,]
#   data.train <- data.6[-testindex,]
#   
# }

# ================Cox Proportional Regression Modeling =============================================
library(survival)
data.9 <- data.8.2
set.seed(10)
data.10 <- data.9[sample(nrow(data.9)),]
train <- data.10[c(1:1078),]
test <- data.10[c(1079:nrow(data.10)),]
names(train)
str(data.10)

coxph_lactate.clear <- coxph(Surv(exit.time, response) ~ ., method = 'efron', data = train[,-80])
summary(coxph_lactate.clear)
tab <- summary(coxph_lactate.clear)
# install.packages('stargazer')
library(stargazer)
exp.coef <- exp(coxph_lactate.clear$coef)
CI.vector <- exp(confint(coxph_lactate.clear))
stargazer(coxph_lactate.clear, coef=list(exp.coef), single.row=T, ci=T, ci.custom=list(CI.vector))

# install.packages('pec')
library('pec')
psurv <- data.frame(predictSurvProb(coxph_lactate.clear,newdata=test,times=seq(6,24,6)))
names(psurv) <- c('0~6hr', '6~12hr', '12~18hr', '18~24hr')
psurv


psurv.2 <- data.frame(apply(psurv, 2, function(x) 1-x))

psurv.2$moretime <- 1 - psurv.2[,4]
psurv.2 <- data.frame(psurv.2)
names(psurv.2) <- c('0~6hr', '0~12hr', '0~18hr', '0~24hr', 'noclear_in_24hrs')
timeranges <- c('0~6hr', '6~12hr', '12~18hr', '18~24hr', 'noclear_in_24hrs')

psurv.2$A <- psurv.2[,2] - psurv.2[,1]
psurv.2$B <- psurv.2[,3] - psurv.2[,2]
psurv.2$C <- psurv.2[,4] - psurv.2[,3]
psurv.3 <- data.frame(psurv.2$`0~6hr`, psurv.2$A, psurv.2$B, psurv.2$C, psurv.2$noclear_in_24hrs)
names(psurv.3) <- timeranges

psurv.2$possibletime <- apply(psurv.3, 1, function(x) timeranges[which.max(x)])
psurv.2$A <- NULL
psurv.2$B <- NULL
psurv.2$C <- NULL
truth.test <- data.9[rownames(psurv.2),c('exit.time', 'response')]
psurv.2$exittime <- truth.test$exit.time
psurv.2$response <- truth.test$response
psurv.2 <- data.frame(psurv.2)
names(psurv.2) <- c('0~6hr', '0~12hr', '0~18hr', '0~24hr', 'noclear_in_24hrs', 'predictTime', 'exittime', 'response')
psurv.2$response <- as.character(psurv.2$response)
str(psurv.2)

library(xtable)
psurv.xtable <- xtable(psurv.2)
toLatex(psurv.xtable, digits=3)



plot(survfit(coxph_lactate.clear, newdata=test[c(1,2,5,6),], conf.int=F), fun='event', xmax=72, mark.time=F, lty=c(1,1,5,6), col = c('red', 'black', 'blue', 'green'), xlab="Hrs", ylab="Normalization Prob.")

legend(40, 0.3, c('Pt1','Pt2', 'Pt3', 'Pt4'), lty=c(1,1,5,6), col = c('red', 'black', 'blue', 'green')) 
install.packages('xtable')
library(xtable)
names(test[c(1,2,5,6),])
col_selected <- c(3,8,13,18,23,28,33,37,41,46,51,56,61,66,71,74:79,81,82)
examplepts <- xtable(test[c(1,2,5,6),col_selected])
toLatex(examplepts, digits=3)


examplepts <- xtable(test[c(1,2,5,6),col_selected[1:8]])
toLatex(examplepts, digits=3)

examplepts <- xtable(test[c(1,2,5,6),col_selected[9:15]])
toLatex(examplepts, digits=3)


examplepts <- xtable(test[c(1,2,5,6),col_selected[16:length(col_selected)]])
toLatex(examplepts, digits=3)



#======================For patients with antibiotics=========================
names(data.9A)
uva.pairs("  ~  `198_v3` + `678_v3` + `211_v3` + `618_v3` + `455_v3` + `4552_v3` + `50002_v3` + `50015_v3` + exit.time", main = "Scatter Plot Matrix for Lactate Clearance", data = data.9A,  labels = c('GCS', 'Temp', 'Pulse', 'Resp.', 'Systolic BP', 'Diastolic BP', 'Base Excess', 'Oxygen Saturation', 'exit.time'))

uva.pairs(" ~  `50019_v3` + `50018_v3` + `50016_v3` + `50439_v3` + `50460_v3` + `50149_v3` + `50159_v3` + exit.time", main = "Scatter Plot Matrix for Lactate Clearance", data = data.9A,  labels = c('PO_2', 'pH Arterial', 'PCO_2', 'Protime', 'Partial Thromboplastin Time', 'Potassium', 'Sodium', 'exit.time'))

uva.pairs(" ~  `age` + `initial.lactate` + `score` + `sapsi_first` + `sofa_first` + `freq` + `itemidG` + `dftime` + exit.time", main = "Scatter Plot Matrix for Lactate Clearance", data = data.9A,  labels = c('age', 'initial.lactate', 'score', 'sapsi_first', 'spfa_first', 'ABX types', 'firstABX', 'ABX timing', 'exit.time'))


library(survival)
data.9 <- data.9A
set.seed(10)
data.10 <- data.9[sample(nrow(data.9)),]
train <- data.10[c(1:601),]
test <- data.10[c(602:nrow(data.10)),]
names(train)

coxph_lactate.clear <- coxph(Surv(exit.time, response) ~ ., method = 'efron', data = train[,-1])
summary(coxph_lactate.clear)
tab <- summary(coxph_lactate.clear)
# install.packages('stargazer')
library(stargazer)
exp.coef <- exp(coxph_lactate.clear$coef)
CI.vector <- exp(confint(coxph_lactate.clear))
stargazer(coxph_lactate.clear, coef=list(exp.coef), single.row=T, ci=T, ci.custom=list(CI.vector))

# install.packages('pec')
library('pec')
psurv <- data.frame(predictSurvProb(coxph_lactate.clear,newdata=test,times=seq(6,24,6)))
names(psurv) <- c('0~6hr', '6~12hr', '12~18hr', '18~24hr')
psurv


psurv.2 <- data.frame(apply(psurv, 2, function(x) 1-x))

psurv.2$moretime <- 1 - psurv.2[,4]
psurv.2 <- data.frame(psurv.2)
names(psurv.2) <- c('0~6hr', '0~12hr', '0~18hr', '0~24hr', 'noclear_in_24hrs')
timeranges <- c('0~6hr', '6~12hr', '12~18hr', '18~24hr', 'noclear_in_24hrs')

psurv.2$A <- psurv.2[,2] - psurv.2[,1]
psurv.2$B <- psurv.2[,3] - psurv.2[,2]
psurv.2$C <- psurv.2[,4] - psurv.2[,3]
psurv.3 <- data.frame(psurv.2$`0~6hr`, psurv.2$A, psurv.2$B, psurv.2$C, psurv.2$noclear_in_24hrs)
names(psurv.3) <- timeranges

psurv.2$possibletime <- apply(psurv.3, 1, function(x) timeranges[which.max(x)])
psurv.2$A <- NULL
psurv.2$B <- NULL
psurv.2$C <- NULL
truth.test <- data.9[rownames(psurv.2),c('exit.time', 'response')]
psurv.2$exittime <- truth.test$exit.time
psurv.2$response <- truth.test$response
psurv.2 <- data.frame(psurv.2)
names(psurv.2) <- c('0~6hr', '0~12hr', '0~18hr', '0~24hr', 'noclear_in_24hrs', 'predictTime', 'exittime', 'response')
psurv.2$response <- as.character(psurv.2$response)
str(psurv.2)

library(xtable)
psurv.xtable <- xtable(psurv.2)
toLatex(psurv.xtable, digits=3)


plot(survfit(coxph_lactate.clear, newdata=test[c(1,2,4,9),], conf.int=F), fun='event', xmax=72, mark.time=F, lty=c(1,1,5,6), col = c('red', 'black', 'blue', 'green'), xlab="Hrs", ylab="Normalization Prob.")

legend(40, 0.3, c('Pt1','Pt2', 'Pt3', 'Pt4'), lty=c(1,1,5,6), col = c('red', 'black', 'blue', 'green')) 
# install.packages('xtable')
library(xtable)
names(test[c(1,2,4,9),])
col_selected <- c(4,9,14,19,24,29,34,38,42,47,52,57,62,67,72,75:85)
examplepts <- xtable(test[c(1,2,4,9),col_selected])
toLatex(examplepts, digits=3)


examplepts <- xtable(test[c(1,2,4,9),col_selected[1:9]])
toLatex(examplepts, digits=3)

examplepts <- xtable(test[c(1,2,4,9),col_selected[10:18]])
toLatex(examplepts, digits=3)


examplepts <- xtable(test[c(1,2,4,9),col_selected[19:length(col_selected)]])
toLatex(examplepts, digits=3)

#======================For patients who had no antibiotics during the stay=========================
names(data.9B)
uva.pairs("  ~  `198_v3` + `678_v3` + `211_v3` + `618_v3` + `455_v3` + `4552_v3` + `50002_v3` + `50015_v3` + exit.time", main = "Scatter Plot Matrix for Lactate Clearance", data = data.9B,  labels = c('GCS', 'Temp', 'Pulse', 'Resp.', 'Systolic BP', 'Diastolic BP', 'Base Excess', 'Oxygen Saturation', 'exit.time'))

uva.pairs(" ~  `50019_v3` + `50018_v3` + `50016_v3` + `50439_v3` + `50460_v3` + `50149_v3` + `50159_v3` + exit.time", main = "Scatter Plot Matrix for Lactate Clearance", data = data.9B,  labels = c('PO_2', 'pH Arterial', 'PCO_2', 'Protime', 'Partial Thromboplastin Time', 'Potassium', 'Sodium', 'exit.time'))

uva.pairs(" ~  `age` + `initial.lactate` + `score` + `sapsi_first` + `sofa_first` + exit.time", main = "Scatter Plot Matrix for Lactate Clearance", data = data.9B,  labels = c('age', 'initial.lactate', 'score', 'sapsi_first', 'spfa_first', 'exit.time'))


library(survival)
data.9 <- data.9B
set.seed(10)
data.10 <- data.9[sample(nrow(data.9)),]
train <- data.10[c(1:134),]
test <- data.10[c(135:nrow(data.10)),]
names(train)

coxph_lactate.clear <- coxph(Surv(exit.time, response) ~ ., method = 'efron', data = train[,-80])
summary(coxph_lactate.clear)
tab <- summary(coxph_lactate.clear)
# install.packages('stargazer')
library(stargazer)
exp.coef <- exp(coxph_lactate.clear$coef)
CI.vector <- exp(confint(coxph_lactate.clear))
stargazer(coxph_lactate.clear, coef=list(exp.coef), single.row=T, ci=T, ci.custom=list(CI.vector))

# install.packages('pec')
library('pec')
psurv <- data.frame(predictSurvProb(coxph_lactate.clear,newdata=test,times=seq(6,24,6)))
names(psurv) <- c('0~6hr', '6~12hr', '12~18hr', '18~24hr')
psurv


psurv.2 <- data.frame(apply(psurv, 2, function(x) 1-x))

psurv.2$moretime <- 1 - psurv.2[,4]
psurv.2 <- data.frame(psurv.2)
names(psurv.2) <- c('0~6hr', '0~12hr', '0~18hr', '0~24hr', 'noclear_in_24hrs')
timeranges <- c('0~6hr', '6~12hr', '12~18hr', '18~24hr', 'noclear_in_24hrs')

psurv.2$A <- psurv.2[,2] - psurv.2[,1]
psurv.2$B <- psurv.2[,3] - psurv.2[,2]
psurv.2$C <- psurv.2[,4] - psurv.2[,3]
psurv.3 <- data.frame(psurv.2$`0~6hr`, psurv.2$A, psurv.2$B, psurv.2$C, psurv.2$noclear_in_24hrs)
names(psurv.3) <- timeranges

psurv.2$possibletime <- apply(psurv.3, 1, function(x) timeranges[which.max(x)])
psurv.2$A <- NULL
psurv.2$B <- NULL
psurv.2$C <- NULL
truth.test <- data.9[rownames(psurv.2),c('exit.time', 'response')]
psurv.2$exittime <- truth.test$exit.time
psurv.2$response <- truth.test$response
psurv.2 <- data.frame(psurv.2)
names(psurv.2) <- c('0~6hr', '0~12hr', '0~18hr', '0~24hr', 'noclear_in_24hrs', 'predictTime', 'exittime', 'response')
psurv.2$response <- as.character(psurv.2$response)
str(psurv.2)

library(xtable)
psurv.xtable <- xtable(psurv.2)
toLatex(psurv.xtable, digits=3)



plot(survfit(coxph_lactate.clear, newdata=test[c(1,2,3,7),], conf.int=F), fun='event', xmax=72, mark.time=F, lty=c(1,1,5,6), col = c('red', 'black', 'blue', 'green'), xlab="Hrs", ylab="Normalization Prob.")

legend(40, 0.3, c('Pt1','Pt2', 'Pt3', 'Pt4'), lty=c(1,1,5,6), col = c('red', 'black', 'blue', 'green')) 
# install.packages('xtable')
library(xtable)
names(test[c(1,2,3,7),])
col_selected <- c(3,8,13,18,23,28,33,37,41,46,51,56,61,66,71,74:79,81,82)
examplepts <- xtable(test[c(1,2,3,7),col_selected])
toLatex(examplepts, digits=3)


examplepts <- xtable(test[c(1,2,3,7),col_selected[1:8]])
toLatex(examplepts, digits=3)

examplepts <- xtable(test[c(1,2,3,7),col_selected[9:15]])
toLatex(examplepts, digits=3)


examplepts <- xtable(test[c(1,2,3,7),col_selected[16:length(col_selected)]])
toLatex(examplepts, digits=3)
