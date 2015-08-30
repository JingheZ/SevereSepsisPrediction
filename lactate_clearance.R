library(lubridate)
library(plyr)
install.packages('ggplot2')
library(ggplot2)
# to get the time of severe sepsis event
load("severe.sepsis2.eventtime.3.RData")
names(severe.sepsis2.eventtime.3)
sepsis.time <- data.frame(severe.sepsis2.eventtime.3$id, severe.sepsis2.eventtime.3$charttime)
names(sepsis.time) <- c('id', 'event.time')

#===================antibiotics=======================================================
antibiotics <- read.csv("sepsisptsdf1_antibiotics.csv")

antibiotics$id <- paste(antibiotics$subject_id, antibiotics$hospital_seq, antibiotics$icustay_seq, sep='#%#')
antibiotics2 <- data.frame(antibiotics$id, antibiotics$start_dt, antibiotics$stop_dt, antibiotics$drug)
names(antibiotics2) <- c('id', 'start_time', 'stop_time', 'itemid')
antibiotics2 <- antibiotics2[(antibiotics2$itemid != 'Meropenem Desensitization') & (antibiotics2$itemid != 'Hepatitis B Immune Globulin (Nabi-HB)'), ]

antibiotics3 <- merge(antibiotics2, sepsis.time, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
antibiotics3$start_time <- ymd_hms(antibiotics3$start_time)
antibiotics3$stop_time <- ymd_hms(antibiotics3$stop_time)
antibiotics3$dftime <- difftime(antibiotics3$start_time, antibiotics3$event.time, units = 'hours')
length(unique(antibiotics3$itemid))
length(unique(antibiotics3$id))
#to get patients who had antibiotics before time of event
antibiotics3a <- antibiotics3[antibiotics3$dftime < 0, ]
length(unique(antibiotics3a$id))
names(antibiotics3)

abtiming <- function(dataset) {
  if (nrow(dataset) > 1) {
    firstab <- dataset[!is.na(dataset[, 4]), 6][1]
    dataset2 <- dataset[dataset[,6] >=0, ]
    firstab.afterevent <- dataset2[!is.na(dataset2[, 4]), 6][1]
  }
  else {
    firstab <- dataset[1,6]
    if ((!is.na(dataset[1,6]) & (dataset[1,6]) >= 0)) {
    firstab.afterevent <- dataset[1,6]
    } else {
      firstab.afterevent <- NA
    }
  }
  ab.times <- c(firstab, firstab.afterevent)
  return(ab.times)
}

antibiotics3 <- antibiotics3[order(antibiotics3$id, antibiotics3$dftime),]
abtimes <- ddply(antibiotics3, .(id), abtiming)
names(abtimes) <- c('id', 'first.abtime', 'firstafterevent.abtime')


#plot the time of antibiotics over the entire ICU stay
# stat_density(mapping = NULL, data = NULL, geom = "area", position = "stack", adjust = 1, 
#              kernel = "gaussian", trim = FALSE, na.rm = FALSE)
# Histogram overlaid with kernel density curve
ggplot(abtimes, aes(x=first.abtime)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=10,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")


ggplot(abtimes, aes(x=first.abtime)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") + xlim(-200, 100)

ggplot(abtimes, aes(x=first.abtime)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=10,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + xlim(-100, 100)

#plot the time of antibiotics after time of event

length(abtimes$id[is.na(abtimes$firstafterevent.abtime)]) #99 patients but have had ab before event; 1340-1103 patients never had antibiotics
ggplot(abtimes, aes(x=firstafterevent.abtime)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")


ggplot(abtimes, aes(x=firstafterevent.abtime)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") + xlim(0, 100)


#draw heatmap for the frequency of patients gettting a type of antibiotics

antibiotics3 <- antibiotics3[order(antibiotics3$id, antibiotics3$dftime),]
unique(antibiotics3$itemid)

#create a dictionary to group antibiotics into more general clusters
Cephalosporins <- c('CeftazIDIME', 'Ceftazidime', 'CeftAZIDime', 'Ceftriaxone', 'CefTRIAXone', 'CeftriaXONE', 'CefePIME', 'Cefepime')
         
Penicillins <- c('Piperacillin-Tazobactam Na', 'Piperacillin Sodium', 'Piperacillin', 'Ampicillin-Sulbactam', 'Ampicillin Sodium', 
                 'Ampicillin Sodium/Sulbactam', 'Unasyn', 'Penicillin G Potassium', 'Nafcillin')

Fluoroquinolones <- c('Ciprofloxacin', 'Ciprofloxacin IV', 'Levofloxacin')


Aminoglycosides <- c('Tobramycin Sulfate', 'Gentamicin Sulfate', 'Gentamicin', 'Amikacin')

Monobactams <- c('Aztreonam', 'Imipenem-Cilastatin', 'Meropenem')

Macrolide <- c('Azithromycin ', 'Erythromycin Lactobionate', 'Erythromycin')

Antiviral <- c('Ribavirin *NF*', 'Acyclovir', 'Acyclovir Sodium')

Antifungal <- c('Amphotericin B', 'Fluconazole')

Others <- c('Tigecycline', 'Doxycycline Hyclate', 'Colistin', 'Linezolid', 'MetRONIDAZOLE (FLagyl)', 'Metronidazole', 'Clindamycin',
            'Clindamycin Phosphate', 'Sulfameth/Trimethoprim', 'Daptomycin', 'Vancomycin', 'Vancomycin HCl', 'Rifampin', 'Quinupristin/Dalfopristin', 'Pentamidine Isethionate', 'Cefotetan')


matrix_Cephalosporins <- data.frame(Cephalosporins, rep('Cephalosporins', length(Cephalosporins)))
names(matrix_Cephalosporins) <- c('itemid', 'itemidG')

matrix_Penicillins <- data.frame(Penicillins, rep('Penicillins', length(Penicillins)))
names(matrix_Penicillins) <- c('itemid', 'itemidG')

matrix_Fluoroquinolones <- data.frame(Fluoroquinolones, rep('Fluoroquinolones', length(Fluoroquinolones)))
names(matrix_Fluoroquinolones) <- c('itemid', 'itemidG')

matrix_Aminoglycosides <- data.frame(Aminoglycosides, rep('Aminoglycosides', length(Aminoglycosides)))
names(matrix_Aminoglycosides) <- c('itemid', 'itemidG')

matrix_Monobactams <- data.frame(Monobactams, rep('Monobactams', length(Monobactams)))
names(matrix_Monobactams) <- c('itemid', 'itemidG')

matrix_Macrolide <- data.frame(Macrolide, rep('Macrolide', length(Macrolide)))
names(matrix_Macrolide) <- c('itemid', 'itemidG')

matrix_Antiviral <- data.frame(Antiviral, rep('Antiviral', length(Antiviral)))
names(matrix_Antiviral) <- c('itemid', 'itemidG')

matrix_Antifungal <- data.frame(Antifungal, rep('Antifungal', length(Antifungal)))
names(matrix_Antifungal) <- c('itemid', 'itemidG')

matrix_Others <- data.frame(Others, rep('Others', length(Others)))
names(matrix_Others) <- c('itemid', 'itemidG')

names(antibiotics3)

antibiotics.group <- rbind(matrix_Cephalosporins, matrix_Penicillins)
antibiotics.group <- rbind(antibiotics.group, matrix_Fluoroquinolones)
antibiotics.group <- rbind(antibiotics.group, matrix_Aminoglycosides)
antibiotics.group <- rbind(antibiotics.group, matrix_Monobactams)
antibiotics.group <- rbind(antibiotics.group, matrix_Macrolide)
antibiotics.group <- rbind(antibiotics.group, matrix_Antiviral)
antibiotics.group <- rbind(antibiotics.group, matrix_Antifungal)
antibiotics.group <- rbind(antibiotics.group, matrix_Others)


library(plyr)
antibiotics3$itemid <- as.character(antibiotics3$itemid)
str(antibiotics.group)
antibiotics.group$itemid <- as.character(antibiotics.group$itemid)
antibiotics4 <- merge(antibiotics3, antibiotics.group, by.x = 'itemid', by.y = 'itemid', all.x = T)




antibiotics4.count <- count(antibiotics4, c('id', 'itemidG'))
str(antibiotics4.count)
names(antibiotics4.count) <- c('id', 'itemidG', 'freq')
unique(antibiotics4$id)
# library(reshape2)
# antibiotics4.count2 <- dcast(antibiotics4.count, id ~ itemidG, value.var = 'freq', fill = 0)
library('RColorBrewer')

myPalette <- colorRampPalette(rev(brewer.pal(10, "Spectral")), space="Lab")

zp1 <- ggplot(antibiotics4.count,
              aes(x = id, y = itemidG, fill = freq), title('Numbers of Antibiotics Over Entire Visit')) + theme(axis.text.y=element_text(size=500))
zp1 <- zp1 + geom_tile()
zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100))
# zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
# zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
# zp1 <- zp1 + coord_equal()
zp1 <- zp1 + theme_bw()
print(zp1)



antibiotics4a <- antibiotics4[is.na(antibiotics4$dftime) | (!is.na(antibiotics4$dftime) & antibiotics4$dftime < 0),]
unique(antibiotics4a$id)
length(unique(antibiotics4a$id[!is.na(antibiotics4a$dftime)]))
str(antibiotics4a)
ptids <- data.frame(c(as.character(unique(antibiotics4$id))))
names(ptids) <- 'id'
str(ptids)
antibiotics4b <- merge(antibiotics4a, ptids, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
str(antibiotics4b)
antibiotics4b.count <- count(antibiotics4b, c('id', 'itemidG'))
str(antibiotics4b.count)

zp2 <- ggplot(antibiotics4b.count,
              aes(x = id, y = itemidG, fill = freq), title('Numbers of Antibiotics Prior to Event')) + theme(axis.text.y=element_text(size=500))
zp2 <- zp2 + geom_tile()
zp2 <- zp2 + scale_fill_gradientn(colours = myPalette(100))
# zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
# zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
# zp1 <- zp1 + coord_equal()
zp2 <- zp2 + theme_bw()
print(zp2)



antibiotics4c <- antibiotics4[is.na(antibiotics4$dftime) | (!is.na(antibiotics4$dftime) & antibiotics4$dftime >= 0),]
length(unique(antibiotics4c$id))
length(unique(antibiotics4c$id[!is.na(antibiotics4c$dftime)]))
str(antibiotics4c)
ptids <- data.frame(c(as.character(unique(antibiotics4$id))))
names(ptids) <- 'id'
str(ptids)
antibiotics4d <- merge(antibiotics4c, ptids, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
str(antibiotics4d)
antibiotics4d.count <- count(antibiotics4d, c('id', 'itemidG'))
str(antibiotics4d.count)

zp3 <- ggplot(antibiotics4d.count,
              aes(x = id, y = itemidG, fill = freq), title('Numbers of Antibiotics After Event')) + theme(axis.text.y = element_text(size=500))
zp3 <- zp3 + geom_tile()
zp3 <- zp3 + scale_fill_gradientn(colours = myPalette(100))
# zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
# zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
# zp1 <- zp1 + coord_equal()
zp3 <- zp3 + theme_bw()
print(zp3)




load("severe.sepsis.RData")
names(severe.sepsis)

severe.sepsis2 <- severe.sepsis[!is.na(severe.sepsis$itemid) & (severe.sepsis$itemid == 50010 | severe.sepsis$itemid == 818 | severe.sepsis$itemid == 1531), c(1:4)]    
lactate.clear <- merge(severe.sepsis2, sepsis.time, by.x = 'id', by.y = 'id', all.x = T)
lactate.clear$lactate.dftime <- difftime(lactate.clear$charttime, lactate.clear$event.time, units = 'hours')

lactate.clear2 <- lactate.clear[lactate.clear$lactate.dftime >= 0,]
lactate.clear3 <- lactate.clear2[!is.na(lactate.clear2$valuenum),]


computeLactateClear <- function(dataset) {
    initial.lactate <- dataset$valuenum[which(dataset$lactate.dftime == 0)[1]]    # x = time of event
    lastlactate <- dataset$lactate.dftime[nrow(dataset)]
    cleartime <- NA
    normalizetime <- NA
    for (i in (1:nrow(dataset))) {
      if (dataset$valuenum[i] <= initial.lactate * 0.9) {
        cleartime <- dataset$lactate.dftime[i]
        break
      }
    }
    for (i in (1:nrow(dataset))) {   
      if (dataset$valuenum[i] <= 2.5) {
        normalizetime <- dataset$lactate.dftime[i]
        break
      }
    }
    lactateclear <- c(initial.lactate, cleartime, normalizetime, lastlactate)
    return(lactateclear)
}
lactate.clear3 <- lactate.clear3[order(lactate.clear3$id, lactate.clear3$lactate.dftime),]
lactateclearance <- ddply(lactate.clear3, .(id), computeLactateClear)
names(lactateclearance) <- c('id', 'initial.lactate', 'cleartime', 'normalizetime', 'lastlactate.dftime')
length(lactateclearance$cleartime[!is.na(lactateclearance$cleartime)]) # 1091
#plot the histogram for lactate 10% clearance time

ggplot(lactateclearance, aes(x=cleartime)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=10,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")


ggplot(lactateclearance, aes(x=cleartime)) + 
  geom_histogram(binwidth=.5, colour="black", fill="white") + xlim(0, 250) 
summary(lactateclearance$cleartime)

ggplot(lactateclearance, aes(x=normalizetime)) + 
  geom_histogram(binwidth=.5, colour="black", fill="white") + xlim(0, 250)
summary(lactateclearance$normalizetime)



#remove the antibiotics taken after lactate clearance
antibiotics5 <- merge(antibiotics4, lactateclearance, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
antibiotics5a <- antibiotics5[(is.na(antibiotics5$cleartime)) | ((!is.na(antibiotics5$cleartime)) & (antibiotics5$dftime < antibiotics5$cleartime)), ]
antibiotics5a <- antibiotics5a[!is.na(antibiotics5a$id),]
length(unique(antibiotics5a$id))
#921


antibiotics5a.count <- count(antibiotics5a, c('id', 'itemidG'))
str(antibiotics5a.count)
unique(antibiotics5a.count$id)

antibiotics5.count.noNA <- antibiotics5.count[!is.na(antibiotics5.count$itemidG),-3]
antibiotics5.numab.count <- count(antibiotics5.count.noNA, 'id')


pts.0ab <- antibiotics5.count.noab
pts.ab <- unique(antibiotics5a$id)

length(unique(antibiotics5a$id))

lactateclearanceA <- lactateclearance[lactateclearance$id %in% pts.0ab, c(1,2,3,5)]

lactateclearanceA$censor <- NA
lactateclearanceA$time <- NA
for (i in 1:nrow(lactateclearanceA)) {
  if (is.na(lactateclearanceA$cleartime[i])) {
    lactateclearanceA$time[i] <- lactateclearanceA$lastlactate.dftime[i]
    lactateclearanceA$censor[i] <- 0
  }  else {
    lactateclearanceA$time[i] <- lactateclearanceA$cleartime[i]
    lactateclearanceA$censor[i] <- 1 
  }
}
lactateclearanceA$drugab <- 0


lactateclearanceB <- lactateclearance[lactateclearance$id %in% pts.ab, c(1,2,3,5)]

lactateclearanceB$censor <- NA
lactateclearanceB$time <- NA
for (i in 1:nrow(lactateclearanceB)) {
  if (is.na(lactateclearanceB$cleartime[i])) {
    lactateclearanceB$time[i] <- lactateclearanceB$lastlactate.dftime[i]
    lactateclearanceB$censor[i] <- 0
  }  else {
    lactateclearanceB$time[i] <- lactateclearanceB$cleartime[i]
    lactateclearanceB$censor[i] <- 1   
  }
}
lactateclearanceB$drugab <- 1

lactateclearance.ab <- rbind(lactateclearanceA, lactateclearanceB)
lactateclearance.ab$drugab <- as.factor(lactateclearance.ab$drugab)
lactateclearance.ab$censor <- as.factor(lactateclearance.ab$censor)
lactateclearance.ab$time <- as.numeric(lactateclearance.ab$time)
str(lactateclearance.ab)


library(survival)
lactateclearance.ab2 <- lactateclearance.ab
str(lactateclearance.ab2)
lactateclearance.ab2$survObj <- with(lactateclearance.ab2, Surv(time, censor==1))
head(lactateclearance.ab2)

lactateclearance.ab2 <- lactateclearance.ab2[(lactateclearance.ab2$id != '25012#%#1#%#1'),]
# surv.as.one <- survfit(survObj ~ 1, data = lactateclearance.ab2, conf.type = "log-log")
surv.as.one <- npsurv(survObj ~ 1, data = lactateclearance.ab2, conf.type = "log-log")
# plot(surv.as.one)

# surv.by.numab <- survfit(survObj ~ drugab, data = lactateclearance.ab2, conf.type = "log-log")
surv.by.numab <- npsurv(survObj ~ drugab, data = lactateclearance.ab2, conf.type = "log-log")

# plot(surv.by.numab, conf = F)
library(rms)
survplot(surv.as.one, xlab = 'hours', ylab = 'lactate no-clearance probability')
survplot(surv.by.numab, xlab = 'hours', ylab = 'lactate no-clearance probability')

survplot(surv.by.numab, what = 'hazard', xlab = 'hours', ylab = 'lactate no-clearance probability')


# Density plots with means
cdat <- ddply(lactateclearance.ab2, "drugab", summarise, rating.mean=mean(initial.lactate))


ggplot(lactateclearance.ab2, aes(x=initial.lactate, colour=drugab)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=drugab),
             linetype="dashed", size=1)

a <- lactateclearance.ab2[lactateclearance.ab2$drugab==0,]


library('RColorBrewer')


ptids$freq2 <- 0
antibiotics5a.count2 <- merge(antibiotics5a.count, ptids, all.x = T, all.Y = T, by.x = 'id', by.y = 'id')

myPalette <- colorRampPalette(rev(brewer.pal(10, "Spectral")), space="Lab")
unique(antibiotics5a.count2$id)

zp1 <- ggplot(antibiotics5a.count,
              aes(x = id, y = itemidG, fill = freq), title('Numbers of Antibiotics Over Entire Visit Before Clearance')) + theme(axis.text.y=element_text(size=500))
zp1 <- zp1 + geom_tile()
zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100))
# zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
# zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
# zp1 <- zp1 + coord_equal()
zp1 <- zp1 + theme_bw()
print(zp1)



antibiotics5a <- antibiotics5[is.na(antibiotics5$dftime) | (!is.na(antibiotics5$dftime) & antibiotics5$dftime < 0),]
unique(antibiotics4a$id)
length(unique(antibiotics4a$id[!is.na(antibiotics4a$dftime)]))
str(antibiotics4a)
ptids <- data.frame(c(as.character(unique(antibiotics4$id))))
names(ptids) <- 'id'
str(ptids)
antibiotics4b <- merge(antibiotics4a, ptids, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
str(antibiotics4b)
antibiotics4b.count <- count(antibiotics4b, c('id', 'itemidG'))
str(antibiotics4b.count)

zp2 <- ggplot(antibiotics4b.count,
              aes(x = id, y = itemidG, fill = freq), title('Numbers of Antibiotics Prior to Event')) + theme(axis.text.y=element_text(size=500))
zp2 <- zp2 + geom_tile()
zp2 <- zp2 + scale_fill_gradientn(colours = myPalette(100))
# zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
# zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
# zp1 <- zp1 + coord_equal()
zp2 <- zp2 + theme_bw()
print(zp2)



antibiotics4c <- antibiotics4[is.na(antibiotics4$dftime) | (!is.na(antibiotics4$dftime) & antibiotics4$dftime >= 0),]
length(unique(antibiotics4c$id))
length(unique(antibiotics4c$id[!is.na(antibiotics4c$dftime)]))
str(antibiotics4c)
ptids <- data.frame(c(as.character(unique(antibiotics4$id))))
names(ptids) <- 'id'
str(ptids)
antibiotics4d <- merge(antibiotics4c, ptids, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
str(antibiotics4d)
antibiotics4d.count <- count(antibiotics4d, c('id', 'itemidG'))
str(antibiotics4d.count)

zp3 <- ggplot(antibiotics4d.count,
              aes(x = id, y = itemidG, fill = freq), title('Numbers of Antibiotics After Event')) + theme(axis.text.y = element_text(size=500))
zp3 <- zp3 + geom_tile()
zp3 <- zp3 + scale_fill_gradientn(colours = myPalette(100))
# zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
# zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
# zp1 <- zp1 + coord_equal()
zp3 <- zp3 + theme_bw()
print(zp3)


















firstantibiotic.bypt.bytype <- function(dataset) {
  first<- dataset[1,]   # x = time of event
  return(first)
}

# antibiotics6 <- ddply(antibiotics5, .(id, itemid), firstantibiotic.bypt.bytype)
antibiotics6B <- ddply(antibiotics5, .(id), firstantibiotic.bypt.bytype)
head(antibiotics6B)
#to model the relationship between time to antibiotic and time to 10% lactate clearance
#hence, remove the obs. in which clearance is earlier than antibiotic is given



















#==============================model the relationship between antibiotics and 10% lactate clearance time (transformed to multi-class) =============================
antibiotics7 <- antibiotics6[(antibiotics6$dftime < antibiotics6$cleartime) | is.na(antibiotics6$cleartime), ]
antibiotics6B$response <- NULL
for (i in 1:nrow(antibiotics6B)) {
  if ((!is.na(antibiotics6B$cleartime[i])) & (antibiotics6B$cleartime[i] <= 6)) {
    antibiotics6B$respone[i] <- '1'
  } else if ((!is.na(antibiotics6B$cleartime[i])) & (antibiotics6B$cleartime[i] <= 12)) {
    antibiotics6B$respone[i] <- '2'
  } else if ((!is.na(antibiotics6B$cleartime[i])) & (antibiotics6B$cleartime[i] <= 24)) {
    antibiotics6B$respone[i] <- '3'
  } else if (((!is.na(antibiotics6B$cleartime[i])) & (antibiotics6B$cleartime[i] > 24)) | ((is.na(antibiotics6B$cleartime[i])) & (antibiotics6B$lastlactate.dftime[i] >= 24))) {
    antibiotics6B$respone[i] <- '4'
  } else if ((is.na(antibiotics6B$cleartime[i])) & (!is.na(antibiotics6B$lastlactate.dftime[i])) & (antibiotics6B$lastlactate.dftime[i] < 24)) {
    antibiotics6B$respone[i] <- '5'
  }
    else {
    antibiotics6B$respone[i] <- '6'
  }
}
antibiotics6B <- antibiotics6B[order(antibiotics6B$respone),]


abdata <- antibiotics6B[, c(1, 4, 6, 7, 10, 11)]
str(abdata)
abdata$dftime <- as.numeric(abdata$dftime)
abdata$respone <- as.character(abdata$respone)
table(abdata$respone)

abdata2 <- abdata[c(1:1140),] # remove those without a valid lactate clear time due to insufficient lactate tests
abdata2$dftime[is.na(abdata2$itemid)] <- 1000
abdata2$itemid2 <- as.character(abdata2$itemid)
abdata2$itemid2[is.na(abdata2$itemid)] <- 'AAAAA'
abdata2$itemid2 <-as.factor(abdata2$itemid2)
abdata2 <- abdata2[order(abdata2$respone),]
head(abdata2)
#predictive models 
library(nnet)
abdata2$respone <- as.factor(abdata2$respone)
abdata2$respone2 <- relevel(abdata2$respone, ref = '4')
test = multinom(respone2 ~ dftime + initial.lactate + itemid2, data = abdata2)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#==========================model the relationship between antibiotics and lactate normalization time (transformed to multi-class) =============================
antibiotics6C$responseB <- NULL
for (i in 1:nrow(antibiotics6B)) {
  if ((!is.na(antibiotics6B$normalizetime[i])) & (antibiotics6B$normalizetime[i] <= 6)) {
    antibiotics6B$responeB[i] <- '1'
  } else if ((!is.na(antibiotics6B$normalizetime[i])) & (antibiotics6B$normalizetime[i] <= 12)) {
    antibiotics6B$responeB[i] <- '2'
  } else if ((!is.na(antibiotics6B$normalizetime[i])) & (antibiotics6B$normalizetime[i] <= 24)) {
    antibiotics6B$responeB[i] <- '3'
  } else if (((!is.na(antibiotics6B$normalizetime[i])) & (antibiotics6B$normalizetime[i] > 24)) | ((is.na(antibiotics6B$normalizetime[i])) & (antibiotics6B$lastlactate.dftime[i] >= 24))) {
    antibiotics6B$responeB[i] <- '4'
  } else if ((is.na(antibiotics6B$normalizetime[i])) & (!is.na(antibiotics6B$lastlactate.dftime[i])) & (antibiotics6B$lastlactate.dftime[i] < 24)) {
    antibiotics6B$responeB[i] <- '5'
  }
  else {
    antibiotics6B$responeB[i] <- '6'
  }
}
antibiotics6B <- antibiotics6B[order(antibiotics6B$responeB),]
head(antibiotics6B)

abdataB <- antibiotics6B[, c(1, 4, 6, 7, 10, 12)]
str(abdataB)
abdataB$dftime <- as.numeric(abdataB$dftime)
abdataB$responeB <- as.character(abdataB$responeB)
table(abdataB$responeB)

abdataB2 <- abdataB[c(1:1026),] # remove those without a valid lactate clear time due to insufficient lactate tests
abdataB2$dftime[is.na(abdataB2$itemid)] <- 1000
abdataB2$itemid2 <- as.character(abdataB2$itemid)
abdataB2$itemid2[is.na(abdataB2$itemid)] <- 'AAAAA'
abdataB2$itemid2 <-as.factor(abdataB2$itemid2)
abdataB2 <- abdataB2[order(abdataB2$responeB),]
head(abdataB2)
#predictive models 
library(nnet)
abdataB2$responeB <- as.factor(abdataB2$responeB)
abdataB2$responeB2 <- relevel(abdataB2$responeB, ref = '4')
testB = multinom(responeB2 ~ dftime + initial.lactate + itemid2, data = abdataB2)
summary(testB)
zB <- summary(testB)$coefficients/summary(testB)$standard.errors
pB <- (1 - pnorm(abs(zB), 0, 1)) * 2
pB


#=============================




#imputation to be done=====================================
# require(DMwR)
# data.l1 <- knnImputation(data.l, k=63, scale=T, meth="weighAvg")

#===================intraveneous fliuds=======================================================
fluids <- read.csv("sepsisptsdf1_fliuds.csv")
fluids$id <- paste(fluids$subject_id, fluids$hospital_seq, fluids$icustay_seq, sep='#%#')
fluids2 <- data.frame(fluids$id, fluids$charttime, fluids$itemid, fluids$volume, fluids$volumeuom)
names(fluids2) <- c('id', 'charttime', 'itemid', 'volume', 'volumeuom')
summary(fluids2$volumeuom)
#remove all with missing or 0 volume
fluids3 <- fluids2[!is.na(fluids2$volume) & (fluids2$volume > 0), ]
#remove column volumeuom since all fluids in fluids3 are in ml
fluids3$volumeuom <- NULL

fluids4 <- merge(fluids3, sepsis.time, by.x = 'id', by.y = 'id', all.x = T)
fluids4$charttime <- ymd_hms(fluids4$charttime)
fluids4$dftime <- difftime(fluids4$charttime, fluids4$event.time, units = 'hours')
fluids5 <- fluids4[fluids4$dftime >= 0, ]
str(fluids5)
summary(fluids5$itemid)
fluids5 <- merge(fluids5, lactateclearance, by.x = 'id', by.y = 'id', all.x = T, all.y = T)

firstfliud.bypt.bytype <- function(dataset) {
  first<- dataset[1,]   # x = time of event
  return(first)
}

fluids6 <- ddply(fluids5, .(id, itemid), firstfluid.bypt.bytype)
fluids6B <- ddply(fluids5, .(id), firstfliud.bypt.bytype)
head(fliuds6B)
#to model the relationship between time to antibiotic and time to 10% lactate clearance
#hence, remove the obs. in which clearance is earlier than fliud is given


#==============================model the relationship between antibiotics and 10% lactate clearance time (transformed to multi-class) =============================ant
fluids6B$response <- NULL
for (i in 1:nrow(fluids6B)) {
  if ((!is.na(fluids6B$cleartime[i])) & (fluids6B$cleartime[i] <= 6)) {
    fluids6B$respone[i] <- '1'
  } else if ((!is.na(fluids6B$cleartime[i])) & (fluids6B$cleartime[i] <= 12)) {
    fluids6B$respone[i] <- '2'
  } else if ((!is.na(fluids6B$cleartime[i])) & (fluids6B$cleartime[i] <= 24)) {
    fluids6B$respone[i] <- '3'
  } else if (((!is.na(fluids6B$cleartime[i])) & (fluids6B$cleartime[i] > 24)) | ((is.na(fluids6B$cleartime[i])) & (fluids6B$lastlactate.dftime[i] >= 24))) {
    fluids6B$respone[i] <- '4'
  } else if ((is.na(fluids6B$cleartime[i])) & (!is.na(fluids6B$lastlactate.dftime[i])) & (fluids6B$lastlactate.dftime[i] < 24)) {
    fluids6B$respone[i] <- '5'
  }
  else {
    fluids6B$respone[i] <- '6'
  }
}
fluids6B <- fluids6B[order(fluids6B$respone),]

head(fluids6B)
fluiddata <- fluids6B[, c(1, 3, 4, 6, 7, 10, 11)]
str(fluiddata)
fluiddata$dftime <- as.numeric(fluiddata$dftime)
fluiddata$respone <- as.character(fluiddata$respone)
fluiddata$itemid <- as.factor(fluiddata$itemid)
table(fluiddata$respone)

fluiddata2 <- fluiddata[c(1:1140),] # remove those without a valid lactate clear time due to insufficient lactate tests
fluiddata2$itemid2 <- fluiddata2$itemid

fluiddata2 <- fluiddata2[order(fluiddata2$id),]
head(fluiddata2)
abdata2 <- abdata2[order(abdata2$id),]
head(abdata2)
abfluids <- merge(abdata2, fluiddata2, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
abfluids2 <- data.frame(abfluids$id, abfluids$initial.lactate.x, abfluids$itemid2.x, abfluids$dftime.x, abfluids$itemid.y, abfluids$volume, abfluids$dftime.y, abfluids$respone.x)
names(abfluids2) <- c('id', 'initial.lactate', 'antibiotic', 'abdftime', 'fluid', 'volume', 'fluiddftime', 'respone')
abfluids2$fluiddftime[is.na(abfluids2$fluiddftime)] <- 1000
abfluids2$fluid2 <- as.character(abfluids2$fluid)
abfluids2$fluid2[is.na(abfluids2$fluid)] <- '00'
abfluids2$fluid2 <-as.factor(abfluids2$fluid2)



#predictive models 
library(nnet)
abfluids2$respone <- as.factor(abfluids2$respone)
abfluids2$respone2 <- relevel(abfluids2$respone, ref = '4')
head(abfluids2)
test = multinom(respone2 ~ antibiotic + abdftime + fluid2 + fluiddftime + initial.lactate, data = abfluids2)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#==========================model the relationship between fluids and lactate normalization time (transformed to multi-class) =============================
fluids6B$responseB <- NULL
for (i in 1:nrow(fluids6B)) {
  if ((!is.na(fluids6B$normalizetime[i])) & (fluids6B$normalizetime[i] <= 6)) {
    fluids6B$responeB[i] <- '1'
  } else if ((!is.na(fluids6B$normalizetime[i])) & (fluids6B$normalizetime[i] <= 12)) {
    fluids6B$responeB[i] <- '2'
  } else if ((!is.na(fluids6B$normalizetime[i])) & (fluids6B$normalizetime[i] <= 24)) {
    fluids6B$responeB[i] <- '3'
  } else if (((!is.na(fluids6B$normalizetime[i])) & (fluids6B$normalizetime[i] > 24)) | ((is.na(fluids6B$normalizetime[i])) & (fluids6B$lastlactate.dftime[i] >= 24))) {
    fluids6B$responeB[i] <- '4'
  } else if ((is.na(fluids6B$normalizetime[i])) & (!is.na(fluids6B$lastlactate.dftime[i])) & (fluids6B$lastlactate.dftime[i] < 24)) {
    fluids6B$responeB[i] <- '5'
  }
  else {
    fluids6B$responeB[i] <- '6'
  }
}
fluids6B <- fluids6B[order(fluids6B$responeB),]

head(fluids6B)
fluiddataB <- fluids6B[, c(1, 3, 4, 6, 7, 10, 12)]
str(fluiddataB)
fluiddataB$dftime <- as.numeric(fluiddataB$dftime)
fluiddataB$responeB <- as.character(fluiddataB$responeB)
fluiddataB$itemid <- as.factor(fluiddataB$itemid)
table(fluiddataB$responeB)

fluiddataB2 <- fluiddataB[c(1:1026),] # remove those without a valid lactate clear time due to insufficient lactate tests
fluiddataB2$itemid2 <- fluiddataB2$itemid

fluiddataB2 <- fluiddataB2[order(fluiddataB2$id),]
head(fluiddataB2)
abdataB2 <- abdataB2[order(abdataB2$id),]
head(abdataB2)
abfluidsB <- merge(abdataB2, fluiddataB2, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
names(abfluidsB)
abfluidsB2 <- data.frame(abfluidsB$id, abfluidsB$initial.lactate.x, abfluidsB$itemid2.x, abfluidsB$dftime.x, abfluidsB$itemid.y, abfluidsB$volume, abfluidsB$dftime.y, abfluidsB$responeB.x)
names(abfluidsB2) <- c('id', 'initial.lactate', 'antibiotic', 'abdftime', 'fluid', 'volume', 'fluiddftime', 'respone')
abfluidsB2$fluiddftime[is.na(abfluidsB2$fluiddftime)] <- 1000
abfluidsB2$fluid2 <- as.character(abfluidsB2$fluid)
abfluidsB2$fluid2[is.na(abfluidsB2$fluid)] <- '00'
abfluidsB2$fluid2 <-as.factor(abfluidsB2$fluid2)



#predictive models 
library(nnet)
abfluidsB2$respone <- as.factor(abfluidsB2$respone)
abfluidsB2$respone2 <- relevel(abfluidsB2$respone, ref = '4')
head(abfluidsB2)
abfluidsB2$id <- as.character(abfluidsB2$id)

testB = multinom(respone2 ~ antibiotic + abdftime + fluid2 + volume + fluiddftime + initial.lactate, data = abfluidsB2)
summary(testB)
zB <- summary(testB)$coefficients/summary(testB)$standard.errors
pB <- (1 - pnorm(abs(zB), 0, 1)) * 2
pB
res = predict(testB, abfluidsB2[1,], 'probs')
res2 = c(res, abfluidsB2[1,1])
res2
res
head(res)
summary(fluids6B$cleartime)

#cross-validation for lactate 10% decrease prediction
lactateclearance.predict <- function(data) {
  predictions <- data.frame(matrix(NA, ncol=4))
  # names(predictions) <- c(1,2,3,4)
  
  ids <- c(NA)
  labels <- c(NA)
  data <- data[sample.int(nrow(data)),]
  for (i in 1:nrow(data)) {
    train <- data[-i, ]
    test <- data[i, ]
    model <- multinom(respone2 ~ antibiotic + abdftime + fluid2 + volume + fluiddftime + initial.lactate, train)
    res <- predict(model, test, 'probs')
    predictions <- rbind(predictions, c(res))
    ids <- c(ids, test[1])
    labels <- c(labels, test[10])
  }
  predictions$id <- ids
  predictions$label <- labels
  predictions <- predictions[-1,]
  return(predictions)
}
summary(abfluidsB2$respone2)
results.clearance0 <- lactateclearance.predict(abfluidsB2[1:8,])
results.normalize <- lactateclearance.predict(abfluids2)
results.clearance$label0 <- rev(abfluidsB2$respone2)
results.clearance2 <- results.clearance[,-6]
summary(data0$label)
#calculate the prediction performance 
classifier.performane <- function(data) {
  names(data) <- c('Cls4', 'Cls1', 'Cls2', 'Cls3', 'id', 'label')
  data$label <- as.character(data$label)
  data$pred <- apply(data[,(1:4)],1,max)
  t1p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '1'), ])
  t1p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '1'), ])
  t1p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '1'), ])
  t1p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '1'), ])
  t2p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '2'), ])
  t2p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '2'), ])
  t2p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '2'), ])
  t2p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '2'), ])
  t3p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '3'), ])
  t3p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '3'), ])
  t3p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '3'), ])
  t3p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '3'), ])
  t4p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '4'), ])
  t4p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '4'), ])
  t4p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '4'), ])
  t4p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '4'), ])
  performance <- c(t1p1, t1p2, t1p3, t1p4, t2p1, t2p2, t2p3, t2p4, t3p1, t3p2, t3p3, t3p4, t4p1, t4p2, t4p3, t4p4)
  return(performance)
}

performance.clearance <- classifier.performane(results.clearance2)
results.clearance$label <- as.character(results.clearance)
summary(results.clearance$label)
head(results.clearance)

#========================================================================================================================================================
for (i in 1:nrow(fluids6B)) {
    if ((!is.na(fluids6B$normalizetime[i])) & (fluids6B$normalizetime[i] <= 12)) {
    fluids6B$responeC[i] <- '1'
  } else if (((!is.na(fluids6B$normalizetime[i])) & (fluids6B$normalizetime[i] > 12)) | ((is.na(fluids6B$normalizetime[i])) & (fluids6B$lastlactate.dftime[i] >= 12))) {
    fluids6B$responeC[i] <- '2'
  } else if ((is.na(fluids6B$normalizetime[i])) & (!is.na(fluids6B$lastlactate.dftime[i])) & (fluids6B$lastlactate.dftime[i] < 12)) {
    fluids6B$responeC[i] <- '3'
  }
  else {
    fluids6B$responeC[i] <- '4'
  }
}
fluids6B <- fluids6B[order(fluids6B$responeC),]

head(fluids6B)
fluiddataB <- fluids6B[, c(1, 3, 4, 6, 7, 10, 11)]
str(fluiddataB)
fluiddataB$dftime <- as.numeric(fluiddataB$dftime)
fluiddataB$responeC <- as.character(fluiddataB$responeC)
fluiddataB$itemid <- as.factor(fluiddataB$itemid)
table(fluiddataB$responeC)

fluiddataB3 <- fluiddataB[c(1:1118),] # remove those without a valid lactate clear time due to insufficient lactate tests
fluiddataB3$itemid2 <- fluiddataB3$itemid

fluiddataB3 <- fluiddataB3[order(fluiddataB3$id),]
head(fluiddataB3)



abdataB3 <- abdataB3[order(abdataB3$id),]
head(abdataB3)

abfluidsC <- merge(abdataB3, fluiddataB3, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
names(abfluidsC)
abfluidsC2 <- data.frame(abfluidsC$id, abfluidsC$initial.lactate.x, abfluidsC$itemid2.x, abfluidsC$dftime.x, abfluidsC$itemid.y, abfluidsC$volume, abfluidsC$dftime.y, abfluidsC$responeC.x)
names(abfluidsC2) <- c('id', 'initial.lactate', 'antibiotic', 'abdftime', 'fluid', 'volume', 'fluiddftime', 'respone')
abfluidsC2$fluiddftime[is.na(abfluidsC2$fluiddftime)] <- 1000
abfluidsC2$fluid2 <- as.character(abfluidsC2$fluid)
abfluidsC2$fluid2[is.na(abfluidsC2$fluid)] <- '00'
abfluidsC2$fluid2 <-as.factor(abfluidsC2$fluid2)



#predictive models 
library(nnet)
abfluidsB2$respone <- as.factor(abfluidsB2$respone)
abfluidsB2$respone2 <- relevel(abfluidsB2$respone, ref = '4')
head(abfluidsB2)
abfluidsB2$id <- as.character(abfluidsB2$id)
testB2 <- glm(respone2 ~ antibiotic + abdftime + fluid2 + volume + fluiddftime + initial.lactate, data = abfluidsB2, family='binomial')

testB = multinom(respone2 ~ antibiotic + abdftime + fluid2 + volume + fluiddftime + initial.lactate, data = abfluidsB2)
summary(testB2)
zB2 <- summary(testB2)$coefficients/summary(testB2)$standard.errors
pB2 <- (1 - pnorm(abs(zB2), 0, 1)) * 2
pB2
res = predict(testB, abfluidsB2[1,], 'probs')
res2 = c(res, abfluidsB2[1,1])
res2
res
head(res)
summary(fluids6B$cleartime)

#cross-validation for lactate 10% decrease prediction
lactateclearance.predict <- function(data) {
  predictions <- data.frame(matrix(NA, ncol=4))
  # names(predictions) <- c(1,2,3,4)
  
  ids <- c(NA)
  labels <- c(NA)
  data <- data[sample.int(nrow(data)),]
  for (i in 1:nrow(data)) {
    train <- data[-i, ]
    test <- data[i, ]
    model <- multinom(respone2 ~ antibiotic + abdftime + fluid2 + volume + fluiddftime + initial.lactate, train)
    res <- predict(model, test, 'probs')
    predictions <- rbind(predictions, c(res))
    ids <- c(ids, test[1])
    labels <- c(labels, test[10])
  }
  predictions$id <- ids
  predictions$label <- labels
  predictions <- predictions[-1,]
  return(predictions)
}
summary(abfluidsB2$respone2)
results.clearance0 <- lactateclearance.predict(abfluidsB2[1:8,])
results.normalize <- lactateclearance.predict(abfluids2)
results.clearance$label0 <- rev(abfluidsB2$respone2)
results.clearance2 <- results.clearance[,-6]
summary(data0$label)
#calculate the prediction performance 
classifier.performane <- function(data) {
  names(data) <- c('Cls4', 'Cls1', 'Cls2', 'Cls3', 'id', 'label')
  data$label <- as.character(data$label)
  data$pred <- apply(data[,(1:4)],1,max)
  t1p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '1'), ])
  t1p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '1'), ])
  t1p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '1'), ])
  t1p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '1'), ])
  t2p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '2'), ])
  t2p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '2'), ])
  t2p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '2'), ])
  t2p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '2'), ])
  t3p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '3'), ])
  t3p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '3'), ])
  t3p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '3'), ])
  t3p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '3'), ])
  t4p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '4'), ])
  t4p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '4'), ])
  t4p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '4'), ])
  t4p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '4'), ])
  performance <- c(t1p1, t1p2, t1p3, t1p4, t2p1, t2p2, t2p3, t2p4, t3p1, t3p2, t3p3, t3p4, t4p1, t4p2, t4p3, t4p4)
  return(performance)
}

performance.clearance <- classifier.performane(results.clearance2)
results.clearance$label <- as.character(results.clearance)
summary(results.clearance$label)
head(results.clearance)

#==============================model the relationship between antibiotics and 10% lactate clearance time (transformed to multi-class) =============================ant
abafluid <- merge(antibiotics6B, fluids6B, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
names(abafluid)

abafluid2 <- data.frame(abafliud$id, abafluid$itemid.x, abafliud$dftime.x, abafliud$itemid.y, abafluid$volume, abafliud$dftime.y, abafliud$initial.lactate.x, abafliud$cleartime.x, abafliud$normalizetime.x, abafliud$lastlactate.dftime.x)
names(abafluid2) <- c('id', 'antibiotic', 'abdftime', 'fluid', 'volume', 'fluiddftime', 'initial.lactate', 'cleartime', 'normalizetime', 'lastlactate.dftime')

abafluid2$abdftime[is.na(abafluid2$antibiotic)] <- 1000
abafluid2$antibiotic2 <- as.character(abafluid2$antibiotic)
abafluid2$antibiotic2[is.na(abafluid2$antibiotic)] <- 'AAAAA'
abafluid2$antibiotic2 <-as.factor(abafluid2$antibiotic2)
head(abafluid2)

abafluid2$fluiddftime[is.na(abafluid2$fluid)] <- 1000
abafluid2$volume[is.na(abafluid2$fluid)] <- 0
abafluid2$fluid2 <- as.character(abafluid2$fluid)
abafluid2$fluid2[is.na(abafluid2$fluid)] <- '00'
abafluid2$fluid2 <-as.factor(abafluid2$fluid2)


for (i in 1:nrow(abafluid2)) {
  if ((!is.na(abafluid2$normalizetime[i])) & (abafluid2$normalizetime[i] <= 12)) {
    abafluid2$responeC[i] <- '1'
  } else if (((!is.na(abafluid2$normalizetime[i])) & (abafluid2$normalizetime[i] > 12)) | ((is.na(abafluid2$normalizetime[i])) & (abafluid2$lastlactate.dftime[i] >= 12))) {
    abafluid2$responeC[i] <- '2'
  } else if ((is.na(abafluid2$normalizetime[i])) & (!is.na(abafluid2$lastlactate.dftime[i])) & (abafluid2$lastlactate.dftime[i] < 12)) {
    abafluid2$responeC[i] <- '3'
  }
  else {
    abafluid2$responeC[i] <- '4'
  }
}
abafluid2 <- abafluid2[order(abafluid2$responeC),]

head(abafluid2)
unique(abafluid2$antibiotic2)

str(abafluid2)
abafluid2$abdftime <- as.numeric(abafluid2$abdftime)
abafluid2$fluiddftime <- as.numeric(abafluid2$fluiddftime)
abafluid2$responeC <- as.character(abafluid2$responeC)
abafluid2$antibiotic2 <- as.factor(abafluid2$antibiotic2)
abafluid2$fluid2 <- as.factor(abafluid2$fluid2)
table(abafluid2$responeC)

abafluid3 <- abafluid2[c(1:1118),] # remove those without a valid lactate clear time due to insufficient lactate tests

head(abafluid3)


#predictive models 
library(nnet)
abafluid3$responeC <- as.factor(abafluid3$responeC)
abafluid3$responeC2 <- relevel(abafluid3$responeC, ref = '2')
str(abafluid3)
abafluid3$id <- as.character(abafluid3$id)

testC <- glm(responeC2 ~ antibiotic2 + abdftime + fluid2 + volume + fluiddftime + initial.lactate, data = abafluid3, family='binomial')
testC2 <- glm(responeC2 ~ antibiotic2 + abdftime + initial.lactate, data = abafluid3, family='binomial')

summary(testC2)
unique(abafluid3$fluid2)
library(aod)
wald.test(b = coef(testC), Sigma = vcov(testC), Terms = 42:114)
dim(confint(testC))
#cross-validation for lactate 10% decrease prediction
lactateclearance.predict <- function(data) {
  predictions <- data.frame(matrix(NA, ncol=2))
  # names(predictions) <- c(1,2,3,4)
  
  ids <- c(NA)
  labels <- c(NA)
  data <- data[sample.int(nrow(data)),]
  for (i in 1:nrow(data)) {
    train <- data[-i, ]
    test <- data[i, ]
    model <- multinom(respone2 ~ antibiotic + abdftime + fluid2 + volume + fluiddftime + initial.lactate, train)
    res <- predict(model, test, 'probs')
    predictions <- rbind(predictions, c(res))
    ids <- c(ids, test[1])
    labels <- c(labels, test[10])
  }
  predictions$id <- ids
  predictions$label <- labels
  predictions <- predictions[-1,]
  return(predictions)
}
summary(abfluidsB2$respone2)
results.clearance0 <- lactateclearance.predict(abfluidsB2[1:8,])
results.normalize <- lactateclearance.predict(abfluids2)
results.clearance$label0 <- rev(abfluidsB2$respone2)
results.clearance2 <- results.clearance[,-6]
summary(data0$label)
#calculate the prediction performance 
classifier.performane <- function(data) {
  names(data) <- c('Cls4', 'Cls1', 'Cls2', 'Cls3', 'id', 'label')
  data$label <- as.character(data$label)
  data$pred <- apply(data[,(1:4)],1,max)
  t1p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '1'), ])
  t1p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '1'), ])
  t1p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '1'), ])
  t1p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '1'), ])
  t2p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '2'), ])
  t2p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '2'), ])
  t2p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '2'), ])
  t2p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '2'), ])
  t3p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '3'), ])
  t3p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '3'), ])
  t3p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '3'), ])
  t3p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '3'), ])
  t4p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '4'), ])
  t4p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '4'), ])
  t4p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '4'), ])
  t4p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '4'), ])
  performance <- c(t1p1, t1p2, t1p3, t1p4, t2p1, t2p2, t2p3, t2p4, t3p1, t3p2, t3p3, t3p4, t4p1, t4p2, t4p3, t4p4)
  return(performance)
}

performance.clearance <- classifier.performane(results.clearance2)
results.clearance$label <- as.character(results.clearance)
summary(results.clearance$label)
head(results.clearance)


abafluid3 <- abafluid2[!is.na(abafluid2$cleartime),]
fit <- lm(cleartime ~ antibiotic2 + abdftime + fluid2 + volume + fluiddftime + initial.lactate, data = abafluid3)
summary(fit)
anova(fit)
plot(fit)


abafluid4 <- abafluid2[!is.na(abafluid2$normalizetime),]
fit2 <- lm(cleartime ~ antibiotic2 + abdftime + initial.lactate, data = abafluid4)
summary(fit2)
anova(fit2)
plot(fit2)


#==================create four patient cohorts: 10% lactate decrease within 0~6 hrs; 6~12 hrs; 0~24 hrs; not cleared within 0~24 hrs;
#================first imputation for those without enough observations using KNN=============














#===================== Cox proportional method to model the probability of 12-hour 10% lactate clearance=================================
clear.pts <- fluids7[(!is.na(antibiotics7$cleartime) & antibiotics7$cleartime <= 12),]
noclear.pts <- antibiotics7[(!is.na(antibiotics7$cleartime) & antibiotics7$cleartime > 12) | (is.na(antibiotics7$cleartime) & antibiotics7$lastlactate.dftime > 12),]
right.censored.pts <- antibiotics7[(is.na(antibiotics7$cleartime) & antibiotics7$lastlactate.dftime <= 12),] # right censoring which can be either end-of-study censoring or loss-to-follow-up censoring

install.packages('MIICD')
library(MIICD)
data(ICCRD)
d <- ICCRD[ICCRD$treatment=='tr1',]
res <- DA.ci(k = 10, m = 10, status = 'status', trans = 1 , data = ICCRD,
             conf.int = TRUE, cens.code = 0 , alpha = 0.05)
res # print(res)
plot(res)


#===================== Cox proportional method to model the probability of 12-hour 10% lactate clearance=================================
clear.pts <- antibiotics7[(!is.na(antibiotics7$cleartime) & antibiotics7$cleartime <= 12),]
noclear.pts <- antibiotics7[(!is.na(antibiotics7$cleartime) & antibiotics7$cleartime > 12) | (is.na(antibiotics7$cleartime) & antibiotics7$lastlactate.dftime > 12),]
censored.pts <- antibiotics7[(is.na(antibiotics7$cleartime) & antibiotics7$lastlactate.dftime <= 12),]

cleardata <- data.frame(t(c(clear.pts$itemid[1], clear.pts$dftime[1], 2, clear.pts$cleartime[1])))
names(cleardata) <- c('itemid', 'dftime', 'status', 'cleartime')
for (i in 2:nrow(clear.pts)) {
  cleardata0 <- c(clear.pts$itemid[i], clear.pts$dftime[i], 2, clear.pts$cleartime[i])
  cleardata <- rbind(cleardata, cleardata0)
}


nocleardata <- data.frame(t(c(noclear.pts$itemid[1], noclear.pts$dftime[1], 1, noclear.pts$lastlactate.dftime[1])))
names(nocleardata) <- c('itemid', 'dftime', 'status', 'cleartime')
for (i in 2:nrow(noclear.pts)) {
  nocleardata0 <- c(noclear.pts$itemid[i], noclear.pts$dftime[i], 1, noclear.pts$lastlactate.dftime[i])
  nocleardata <- rbind(nocleardata, nocleardata0)
}


censordata <- data.frame(t(c(censored.pts$itemid[1], censored.pts$dftime[1], 0, censored.pts$lastlactate.dftime[1])))
if (censored.pts$lastlactate.dftime[1] > censored.pts$dftime[1]) {
  censordata <- data.frame(t(c(censored.pts$itemid[1], censored.pts$dftime[1], 0, censored.pts$lastlactate.dftime[1])))
} else {
  censordata <- data.frame(t(c(censored.pts$itemid[1], censored.pts$dftime[1], 0, censored.pts$dftime[1])))
}

names(censordata) <- c('itemid', 'dftime', 'status', 'cleartime')
for (i in 2:nrow(censored.pts)) {
  if (censored.pts$lastlactate.dftime[i] > censored.pts$dftime[i]) {
    censordata0 <- c(censored.pts$itemid[i], censored.pts$dftime[i], 0, censored.pts$lastlactate.dftime[i])
  } else {
    censordata0 <- c(censored.pts$itemid[i], censored.pts$dftime[i], 0, censored.pts$dftime[i])
  }
  censordata <- rbind(censordata, censordata0)
}


modeldata <- rbind(cleardata, nocleardata, censordata)
modeldata$start <- 0
modeldata$itemid <- as.factor(modeldata$itemid)
modeldata$status <- as.factor(modeldata$status)
summary(coxph(Surv(start, cleartime, status), modeldata)) 
# summary(coxph(Surv(start, cleartime, status) ~ (itemid + dftime), modeldata)) 
summary(modeldata)

modeldata0 <- modeldata[modeldata$cleartime ==0,]

