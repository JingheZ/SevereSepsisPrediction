library(lubridate)
library(plyr)
# install.packages('ggplot2')
library(ggplot2)
# to get the time of severe sepsis event
load("severe.sepsis2.eventtime.3.RData")
names(severe.sepsis2.eventtime.3)
sepsis.time <- data.frame(severe.sepsis2.eventtime.3$id, severe.sepsis2.eventtime.3$charttime)
names(sepsis.time) <- c('id', 'event.time')

# read severe.sepsis data
load("severe.sepsis.RData")
names(severe.sepsis)


severe.sepsis2 <- severe.sepsis[!is.na(severe.sepsis$itemid) & (severe.sepsis$itemid == 50010 | severe.sepsis$itemid == 818 | severe.sepsis$itemid == 1531), c(1:4)]    


#===================antibiotics=======================================================
antibiotics <- read.csv("sepsisptsdf1_antibiotics.csv")

antibiotics$id <- paste(antibiotics$subject_id, antibiotics$hospital_seq, antibiotics$icustay_seq, sep='#%#')
antibiotics2 <- data.frame(antibiotics$id, antibiotics$start_dt, antibiotics$stop_dt, antibiotics$drug)
names(antibiotics2) <- c('id', 'start_time', 'stop_time', 'itemid')
antibiotics2 <- antibiotics2[(antibiotics2$itemid != 'Meropenem Desensitization') & (antibiotics2$itemid != 'Hepatitis B Immune Globulin (Nabi-HB)'), ]
#merge antibiotics with severe sepsis time
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

antibiotics3 <- antibiotics3[order(antibiotics3$id, antibiotics3$dftime),]
unique(antibiotics3$itemid)

#create a dictionary to group antibiotics into more general clusters
Cephalosporins <- c('CeftazIDIME', 'Ceftazidime', 'CeftAZIDime', 'Ceftriaxone', 'CefTRIAXone', 'CeftriaXONE', 'CefePIME', 'Cefepime')
         
Penicillins <- c('Piperacillin-Tazobactam Na', 'Piperacillin Sodium', 'Piperacillin', 'Ampicillin-Sulbactam', 'Ampicillin Sodium', 
                 'Ampicillin Sodium/Sulbactam', 'Unasyn', 'Penicillin G Potassium', 'Nafcillin')

Fluoroquinolones <- c('Ciprofloxacin', 'Ciprofloxacin IV', 'Levofloxacin')

Aminoglycosides <- c('Tobramycin Sulfate', 'Gentamicin Sulfate', 'Gentamicin', 'Amikacin')

Carbapenems<- c('Imipenem-Cilastatin', 'Meropenem')

Macrolide <- c('Azithromycin ', 'Erythromycin Lactobionate', 'Erythromycin')

Antiviral <- c('Ribavirin *NF*', 'Acyclovir', 'Acyclovir Sodium')

Antifungal <- c('Amphotericin B', 'Fluconazole')

Others <- c('Aztreonam', 'Tigecycline', 'Doxycycline Hyclate', 'Colistin', 'Linezolid', 'MetRONIDAZOLE (FLagyl)', 'Metronidazole', 'Clindamycin',
            'Clindamycin Phosphate', 'Sulfameth/Trimethoprim', 'Daptomycin', 'Rifampin', 'Quinupristin/Dalfopristin', 'Pentamidine Isethionate', 'Cefotetan')

Vancomycin <- c('Vancomycin', 'Vancomycin HCl')

matrix_Cephalosporins <- data.frame(Cephalosporins, rep('Cephalosporins', length(Cephalosporins)))
names(matrix_Cephalosporins) <- c('itemid', 'itemidG')

matrix_Penicillins <- data.frame(Penicillins, rep('Penicillins', length(Penicillins)))
names(matrix_Penicillins) <- c('itemid', 'itemidG')

matrix_Fluoroquinolones <- data.frame(Fluoroquinolones, rep('Fluoroquinolones', length(Fluoroquinolones)))
names(matrix_Fluoroquinolones) <- c('itemid', 'itemidG')

matrix_Aminoglycosides <- data.frame(Aminoglycosides, rep('Aminoglycosides', length(Aminoglycosides)))
names(matrix_Aminoglycosides) <- c('itemid', 'itemidG')

matrix_Carbapenems <- data.frame(Carbapenems, rep('Carbapenems', length(Carbapenems)))
names(matrix_Carbapenems) <- c('itemid', 'itemidG')

matrix_Macrolide <- data.frame(Macrolide, rep('Macrolide', length(Macrolide)))
names(matrix_Macrolide) <- c('itemid', 'itemidG')

matrix_Antiviral <- data.frame(Antiviral, rep('Antiviral', length(Antiviral)))
names(matrix_Antiviral) <- c('itemid', 'itemidG')

matrix_Antifungal <- data.frame(Antifungal, rep('Antifungal', length(Antifungal)))
names(matrix_Antifungal) <- c('itemid', 'itemidG')

matrix_Others <- data.frame(Others, rep('Others', length(Others)))
names(matrix_Others) <- c('itemid', 'itemidG')

matrix_Vancomycin <- data.frame(Vancomycin, rep('Vancomycin', length(Vancomycin)))
names(matrix_Vancomycin) <- c('itemid', 'itemidG')

names(antibiotics3)

antibiotics.group <- rbind(matrix_Cephalosporins, matrix_Penicillins)
antibiotics.group <- rbind(antibiotics.group, matrix_Fluoroquinolones)
antibiotics.group <- rbind(antibiotics.group, matrix_Aminoglycosides)
antibiotics.group <- rbind(antibiotics.group, matrix_Carbapenems)
antibiotics.group <- rbind(antibiotics.group, matrix_Macrolide)
antibiotics.group <- rbind(antibiotics.group, matrix_Antiviral)
antibiotics.group <- rbind(antibiotics.group, matrix_Antifungal)
antibiotics.group <- rbind(antibiotics.group, matrix_Others)
antibiotics.group <- rbind(antibiotics.group, matrix_Vancomycin)

library(plyr)
antibiotics3$itemid <- as.character(antibiotics3$itemid)
str(antibiotics.group)
antibiotics.group$itemid <- as.character(antibiotics.group$itemid)

#create antibiotics4 to add a column with generalized antibiotics names
antibiotics4 <- merge(antibiotics3, antibiotics.group, by.x = 'itemid', by.y = 'itemid', all.x = T)

#compute the time of the first antibiotics over the entire ICU stay and the time of the first antibiotics after the diagnosis of severe sepsis
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

antibiotics4 <- antibiotics4[order(antibiotics4$id, antibiotics4$dftime),]
abtimes <- ddply(antibiotics4, .(id), abtiming)
names(abtimes) <- c('id', 'first.abtime', 'firstafterevent.abtime')
save(abtimes, file = 'abtimes.RData')


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

length(abtimes$id[is.na(abtimes$firstafterevent.abtime)]) #99 patients but have had ab before event; 1340-1103=337 patients never had antibiotics
ggplot(abtimes, aes(x=firstafterevent.abtime)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")


ggplot(abtimes, aes(x=firstafterevent.abtime)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") + xlim(0, 100)


# get the counts of different antibiotics of each patient
# antibiotics4.count <- count(antibiotics4, c('id', 'itemidG'))
# str(antibiotics4.count)
# names(antibiotics4.count) <- c('id', 'itemidG', 'freq')
# unique(antibiotics4$id)
# 
# 
# # library(reshape2)
# # antibiotics4.count2 <- dcast(antibiotics4.count, id ~ itemidG, value.var = 'freq', fill = 0)
# library('RColorBrewer')
# 
# myPalette <- colorRampPalette(rev(brewer.pal(10, "Spectral")), space="Lab")
# 
# zp1 <- ggplot(antibiotics4.count,
#               aes(x = id, y = itemidG, fill = freq), title('Numbers of Antibiotics Over Entire Visit')) + theme(axis.text.y=element_text(size=500))
# zp1 <- zp1 + geom_tile()
# zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100))
# # zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
# # zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
# # zp1 <- zp1 + coord_equal()
# zp1 <- zp1 + theme_bw()
# print(zp1)
# 
# 
# 
# antibiotics4a <- antibiotics4[is.na(antibiotics4$dftime) | (!is.na(antibiotics4$dftime) & antibiotics4$dftime < 0),]
# unique(antibiotics4a$id)
# length(unique(antibiotics4a$id[!is.na(antibiotics4a$dftime)]))
# str(antibiotics4a)
# ptids <- data.frame(c(as.character(unique(antibiotics4$id))))
# names(ptids) <- 'id'
# str(ptids)
# antibiotics4b <- merge(antibiotics4a, ptids, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
# str(antibiotics4b)
# antibiotics4b.count <- count(antibiotics4b, c('id', 'itemidG'))
# str(antibiotics4b.count)
# 
# zp2 <- ggplot(antibiotics4b.count,
#               aes(x = id, y = itemidG, fill = freq), title('Numbers of Antibiotics Prior to Event')) + theme(axis.text.y=element_text(size=500))
# zp2 <- zp2 + geom_tile()
# zp2 <- zp2 + scale_fill_gradientn(colours = myPalette(100))
# # zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
# # zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
# # zp1 <- zp1 + coord_equal()
# zp2 <- zp2 + theme_bw()
# print(zp2)
# 
# 
# 
# antibiotics4c <- antibiotics4[is.na(antibiotics4$dftime) | (!is.na(antibiotics4$dftime) & antibiotics4$dftime >= 0),]
# length(unique(antibiotics4c$id))
# length(unique(antibiotics4c$id[!is.na(antibiotics4c$dftime)]))
# str(antibiotics4c)
# ptids <- data.frame(c(as.character(unique(antibiotics4$id))))
# names(ptids) <- 'id'
# str(ptids)
# antibiotics4d <- merge(antibiotics4c, ptids, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
# str(antibiotics4d)
# antibiotics4d.count <- count(antibiotics4d, c('id', 'itemidG'))
# str(antibiotics4d.count)
# 
# zp3 <- ggplot(antibiotics4d.count,
#               aes(x = id, y = itemidG, fill = freq), title('Numbers of Antibiotics After Event')) + theme(axis.text.y = element_text(size=500))
# zp3 <- zp3 + geom_tile()
# zp3 <- zp3 + scale_fill_gradientn(colours = myPalette(100))
# # zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
# # zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
# # zp1 <- zp1 + coord_equal()
# zp3 <- zp3 + theme_bw()
# print(zp3)
# 
names(severe.sepsis2)
names(sepsis.time)
#merge severe sepsis data with the time of severe sepsis to compute the lacate clearance time 
lactate.clear <- merge(severe.sepsis2, sepsis.time, by.x = 'id', by.y = 'id', all.x = T)
lactate.clear$lactate.dftime <- difftime(lactate.clear$charttime, lactate.clear$event.time, units = 'hours')

lactate.clear2 <- lactate.clear[lactate.clear$lactate.dftime >= 0,]
lactate.clear3 <- lactate.clear2[!is.na(lactate.clear2$valuenum),]
names(lactate.clear3)

computeLactateClear <- function(dataset) {
    initial.lactate <- dataset$valuenum[which(dataset$lactate.dftime == 0)[1]]    # x = time of event
    lastlactate <- dataset$lactate.dftime[nrow(dataset)]
    cleartime <- NA

    normalizetime <- NA

    for (i in (1:nrow(dataset))) {
      if (dataset$valuenum[i] <= initial.lactate * 0.9) {
        cleartime <- dataset$lactate.dftime[i]
        cleartime0 <- dataset$charttime[i]
        break
      }
      else {
        cleartime0 <- NA
      }
    }
    for (i in (1:nrow(dataset))) {   
      if (dataset$valuenum[i] <= 2.5) {
        normalizetime <- dataset$lactate.dftime[i]
        normalizetime0 <- dataset$charttime[i]
        break
      }
      else {
        normalizetime0 <- NA
      }
    }
    lactateclear <- c(initial.lactate, cleartime, normalizetime, lastlactate, cleartime0, normalizetime0)
    return(lactateclear)
}

lactate.clear3 <- lactate.clear3[order(lactate.clear3$id, lactate.clear3$lactate.dftime),]
lactateclearance <- ddply(lactate.clear3, .(id), computeLactateClear)
names(lactateclearance) <- c('id', 'initial.lactate', 'cleartime', 'normalizetime', 'lastlactate.dftime', 'cleartime0', 'normalizetime0')
save(lactateclearance, file = 'lactateclearance.RData')

#number of patients with 10% cleared lactate
length(lactateclearance$cleartime[!is.na(lactateclearance$cleartime)]) # 1091
#number of patients with normalized lactate
length(lactateclearance$normalizetime[!is.na(lactateclearance$normalizetime)]) # 855

ptid.noclear <- lactateclearance$id[is.na(lactateclearance$cleartime)]
#plot the histogram for lactate 10% clearance time

#mortality 
mortalityinfos2 <- read.csv('pticu_infos.csv', header = T)
mortalityinfos2$id <- paste(mortalityinfos2$subject_id, mortalityinfos2$hospital_seq, mortalityinfos2$icustay_seq, sep='#%#')
names(mortalityinfos2)
#patients expired in the ICU
ptid.icu_mortality <- mortalityinfos2$id[!is.na(mortalityinfos2$icustay_expire_flg) & mortalityinfos2$icustay_expire_flg == 'Y']
ptid.hosp_mortality <- mortalityinfos2$id[!is.na(mortalityinfos2$hospital_expire_flg) & mortalityinfos2$hospital_expire_flg == 'Y']

#patients who did not clear their lactate
ptid.noclear.icu_mortality <- intersect(ptid.noclear, ptid.icu_mortality) #170 patients 
ptid.noclear.icu_survive <- setdiff(ptid.noclear, ptid.icu_mortality) #79 patients
head(ptid.noclear.icu_survive)

ptid.noclear.hosp_mortality <- intersect(ptid.noclear, ptid.hosp_mortality) #185 patients 
ptid.noclear.hosp_survive <- setdiff(ptid.noclear, ptid.hosp_mortality) #64 patients
head(ptid.noclear.hosp_survive)


load('severe.sepsis.timeofevent.RData')
df1.timeofevent <- severe.sepsis.timeofevent
str(mortalityinfos2)
mortalityinfos2$dod <- ymd_hms(mortalityinfos2$dod)
df1.mortality30 <- merge(df1.timeofevent, mortalityinfos2, by.x = 'id', by.y = 'id', all.x = T)
df1.mortality30 <- df1.mortality30[!is.na(df1.mortality30$dod),]
str(df1.mortality30)
df1.mortality30.id <- c()
for (i in 1:nrow(df1.mortality30)) 
  if (!is.na(df1.mortality30$dod[i]) & difftime(df1.mortality30$dod[i], df1.mortality30$time.event[i], units="hours") < 30*24) {
    df1.mortality30.id <- c(df1.mortality30.id, as.character(df1.mortality30$id[i]))
  }



#plot the histogram of lactate clearance time
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



#join the table of the antibiotics and lactate clearance time table
antibiotics5 <- merge(antibiotics4, lactateclearance, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
names(antibiotics5)
names(severe.sepsis2)
antibiotics5.2 <- data.frame(antibiotics5$id, antibiotics5$itemidG, antibiotics5$start_time, antibiotics5$stop_time, 
                             antibiotics5$event.time, antibiotics5$itemid, antibiotics5$cleartime0, antibiotics5$normalizetime0)
names(antibiotics5.2) <- c('id', 'itemid', 'charttime', 'stop_time', 'event.time', 'itemid0', 'cleartime', 'normalizationtime')
antibiotics5.2$valuenum <- NA

severe.sepsis2$stop_time <- severe.sepsis2$charttime
severe.sepsis2$itemid <- as.character(severe.sepsis2$itemid)
severe.sepsis2$event.time <- NA
severe.sepsis2$itemid0 <- NA
severe.sepsis2$cleartime <- NA
severe.sepsis2$normalizationtime <- NA

antibiotics5.lactate.2 <- rbind(antibiotics5.2, severe.sepsis2)
antibiotics5.lactate.2 <- antibiotics5.lactate.2[order(antibiotics5.lactate.2$id, antibiotics5.lactate.2$charttime),] 

antibiotics5.lactate.2$cleartime2 <- as.Date.POSIXct(antibiotics5.lactate.2$cleartime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
antibiotics5.lactate.2$normalizeationtime2 <- as.Date.POSIXct(antibiotics5.lactate.2$normalizationtime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
names(antibiotics5.lactate.2)
antibiotics5.lactate.3 <- data.frame(antibiotics5.lactate.2$id, antibiotics5.lactate.2$itemid, antibiotics5.lactate.2$charttime, 
                                     antibiotics5.lactate.2$stop_time, antibiotics5.lactate.2$valuenum, antibiotics5.lactate.2$event.time, 
                                     antibiotics5.lactate.2$cleartime2, antibiotics5.lactate.2$normalizeationtime2, antibiotics5.lactate.2$itemid0)

names(antibiotics5.lactate.3) <- c('id', 'itemid', 'charttime', 'stop_time', 'valuenum', 'sepsis.time', 'cleartime', 'normalize_time', 'antibiotic_name0')

#=================================================10% lactate clearance===========================================================================================
#remove the antibiotics given after 10% decrease
antibiotics5a <- antibiotics5[(is.na(antibiotics5$cleartime)) | ((!is.na(antibiotics5$cleartime)) & (antibiotics5$dftime < antibiotics5$cleartime)), ]
antibiotics5a <- antibiotics5a[!is.na(antibiotics5a$id),]
length(unique(antibiotics5a$id))
#921    

#aggregate to counts by ptid by antibiotic category
antibiotics5a.count <- count(antibiotics5a, c('id', 'itemidG'))
str(antibiotics5a.count)
unique(antibiotics5a.count$id)

#further aggregate to get patients with antibiotics
antibiotics5a.count.noNA <- antibiotics5a.count[!is.na(antibiotics5a.count$itemidG),-3]
antibiotics5a.numab.count <- count(antibiotics5a.count.noNA, 'id')
length(unique(antibiotics5a.numab.count$id)) #849


#patients with antibiotics before 10% lactate clearance
pts.ab <- unique(antibiotics5a.numab.count$id) #849
#patients without any antibiotics before 10% lactate clearance
pts.0ab <- setdiff(severe.sepsis2.eventtime.3$id, pts.ab) #491



#keep patients the antibiotics given after sepsis and before 10% decrease 
antibiotics5b <- antibiotics5[(is.na(antibiotics5$cleartime)) | ((!is.na(antibiotics5$cleartime)) & (antibiotics5$dftime < antibiotics5$cleartime) & (antibiotics5$dftime >= 0)), ]
antibiotics5b <- antibiotics5b[!is.na(antibiotics5b$id),]
length(unique(antibiotics5b$id))
#686   

#aggregate to counts by ptid by antibiotic category
antibiotics5b.count <- count(antibiotics5b, c('id', 'itemidG'))
str(antibiotics5b.count)
unique(antibiotics5b.count$id)

#further aggregate to get patients with antibiotics
antibiotics5b.count.noNA <- antibiotics5b.count[!is.na(antibiotics5b.count$itemidG),-3]
antibiotics5b.numab.count <- count(antibiotics5b.count.noNA, 'id')
length(unique(antibiotics5b.numab.count$id)) #614


#patients with antibiotics before 10% lactate clearance
pts.b.ab <- unique(antibiotics5b.numab.count$id) 
#patients without any antibiotics before 10% lactate clearance
pts.b.0ab <- pts.0ab

#patients had first antibiotics much earlier before event (refer to the antibiotic time by abtimes) 
pt32788 <- antibiotics5.lactate.3[antibiotics5.lactate.3$id == '32788#%#1#%#1',] #-900.56667 hours # survive
save(pt32788, file = 'pt32788.RData')
pt11923 <- antibiotics5.lactate.3[antibiotics5.lactate.3$id == '11923#%#1#%#1',] #-655.25000 hours # survive
save(pt11923, file = 'pt11923.RData')

#patients had first antibiotics slightly earlier before event (refer to the antibiotic time by abtimes) 
pt17766 <- antibiotics5.lactate.3[antibiotics5.lactate.3$id == '17766#%#1#%#1',] #-900.56667 hours # survive
save(pt17766, file = 'pt17766.RData')

pt25946 <- antibiotics5.lactate.3[antibiotics5.lactate.3$id == '25946#%#1#%#1',] #-900.56667 hours # survive
save(pt25946, file = 'pt25946.RData')


#patients had first antibiotics much late after event (refer to the antibiotic time by abtimes) 
pt30425 <- antibiotics5.lactate.3[antibiotics5.lactate.3$id == '30425#%#1#%#1',] # 294.15000 hours # survive
save(pt30425, file = 'pt30425.RData')
pt8566 <- antibiotics5.lactate.3[antibiotics5.lactate.3$id == '8566#%#1#%#1',] # 463.25000 hours # survive
save(pt8566, file = 'pt8566.RData')
pt5962 <- antibiotics5.lactate.3[antibiotics5.lactate.3$id == '5962#%#1#%#3',] # 200.16667 hours # survive
save(pt5962, file = 'pt5962.RData')


#pts.0ab: patient ids of whom had never had antibiotics before the 10% decrease
head(pts.0ab) #need to redo this and analyze the patient characteristics 
pt10004 <- antibiotics5.lactate.3[antibiotics5.lactate.3$id == '10004#%#1#%#1',] # survive
save(pt10004, file = 'pt10004.RData')

pt15918 <- antibiotics5.lactate.3[antibiotics5.lactate.3$id == '15918#%#1#%#1',] # survive
save(pt15918, file = 'pt15918.RData')

pt12643 <- antibiotics5.lactate.3[antibiotics5.lactate.3$id == '12643#%#1#%#1',] # survived in this ICU stay and discharged but died a year later (80+)
save(pt12643, file = 'pt12643.RData')

pt27237 <- antibiotics5.lactate.3[antibiotics5.lactate.3$id == '27237#%#1#%#1',] # survive
save(pt27237, file = 'pt27237.RData')

pt20283 <- antibiotics5.lactate.3[antibiotics5.lactate.3$id == '20283#%#1#%#1',]  #died
save(pt20283, file = 'pt20283.RData')

#pts.ab: patient ids of whom had antibiotics before the 10% decrease
head(pts.ab)
pt10019 <- antibiotics5.lactate.3[antibiotics5.lactate.3$id == '10019#%#1#%#1',]  #died
save(pt10019, file = 'pt10019.RData')

pt12740 <- antibiotics5.lactate.3[antibiotics5.lactate.3$id == '12740#%#1#%#1',]  #died
save(pt12740, file = 'pt12740.RData')

pt25498 <- antibiotics5.lactate.3[antibiotics5.lactate.3$id == '25498#%#1#%#1',] # survived in this ICU stay but died after 1 month (5 days after discharged from hospital)
save(pt25498, file = 'pt25498.RData')


# patients without sufficient lactate test and not due to death
ptid.noclear.icu_survive
ptid.noclear.hosp_survive
#patients discharged from the ICU or hospital early and died with a couple months or days
pt12064 <- antibiotics5.lactate.3[antibiotics5.lactate.3$id == '12064#%#1#%#1',]  #died
save(pt12064, file = 'pt12064.RData')

pt12292 <- antibiotics5.lactate.3[antibiotics5.lactate.3$id == '12292#%#2#%#1',]  #died
save(pt12292, file = 'pt12292.RData')

pt13575 <- antibiotics5.lactate.3[antibiotics5.lactate.3$id == '13575#%#1#%#1',]  #died
save(pt13575, file = 'pt13575.RData')


#mortality among patients who had or did not have antibiotics before lactate clearance by 10%
ptid.ab.icu_mortality <- intersect(pts.ab, ptid.icu_mortality) #364 patients 
ptid.ab.icu_survive <- setdiff(pts.ab, ptid.icu_mortality) #485 patients

ptid.0ab.icu_mortality <- intersect(pts.0ab, ptid.icu_mortality) #161 patients 
ptid.0ab.icu_survive <- setdiff(pts.0ab, ptid.icu_mortality) #330 patients

length(ptid.ab.icu_mortality) / length(pts.ab)
length(ptid.0ab.icu_mortality) / length(pts.0ab)

ptid.ab.hosp_mortality <- intersect(pts.ab, ptid.hosp_mortality) #426 patients 
ptid.ab.hosp_survive <- setdiff(pts.ab, ptid.hosp_mortality) #423 patients

ptid.0ab.hosp_mortality <- intersect(pts.0ab, ptid.hosp_mortality) #196 patients 
ptid.0ab.hosp_survive <- setdiff(pts.0ab, ptid.hosp_mortality) #295 patients

library(stringr)
hospids <- function(ids) {
  ids2 <- lapply(ids, function (x) str_sub(x, 1, -5))
  ids2 <- unique(unlist(ids2))
  return(ids2)
}


pts.ab.id2 <- hospids(pts.ab)
pts.0ab.id2 <- hospids(pts.0ab)

length(ptid.ab.hosp_mortality) / length(pts.ab.id2)
length(ptid.0ab.hosp_mortality) / length(pts.0ab.id2)


ptid.ab.30_mortality <- intersect(pts.ab, df1.mortality30.id) #435 patients 
ptid.ab.30_survive <- setdiff(pts.ab, df1.mortality30.id) #414 patients

ptid.0ab.30_mortality <- intersect(pts.0ab, df1.mortality30.id) #205 patients 
ptid.0ab.30_survive <- setdiff(pts.0ab, df1.mortality30.id) #286 patients

length(ptid.ab.30_mortality) / length(pts.ab)
length(ptid.0ab.30_mortality) / length(pts.0ab)


#mortality among patients who had or did not have antibiotics before lactate clearance by 10%
ptid.b.ab.icu_mortality <- intersect(pts.b.ab, ptid.icu_mortality) #364 patients 
ptid.b.ab.icu_survive <- setdiff(pts.b.ab, ptid.icu_mortality) #485 patients

ptid.0ab.icu_mortality <- intersect(pts.0ab, ptid.icu_mortality) #161 patients 
ptid.0ab.icu_survive <- setdiff(pts.0ab, ptid.icu_mortality) #330 patients

length(ptid.b.ab.icu_mortality) / length(pts.b.ab)
length(ptid.0ab.icu_mortality) / length(pts.0ab)

ptid.b.ab.hosp_mortality <- intersect(pts.b.ab, ptid.hosp_mortality) #426 patients 
ptid.b.ab.hosp_survive <- setdiff(pts.b.ab, ptid.hosp_mortality) #423 patients

ptid.0ab.hosp_mortality <- intersect(pts.0ab, ptid.hosp_mortality) #196 patients 
ptid.0ab.hosp_survive <- setdiff(pts.0ab, ptid.hosp_mortality) #295 patients

library(stringr)
hospids <- function(ids) {
  ids2 <- lapply(ids, function (x) str_sub(x, 1, -5))
  ids2 <- unique(unlist(ids2))
  return(ids2)
}


pts.b.ab.id2 <- hospids(pts.b.ab)
pts.0ab.id2 <- hospids(pts.0ab)

length(ptid.b.ab.hosp_mortality) / length(pts.b.ab.id2)
length(ptid.0ab.hosp_mortality) / length(pts.0ab.id2)


ptid.b.ab.30_mortality <- intersect(pts.b.ab, df1.mortality30.id) #435 patients 
ptid.b.ab.30_survive <- setdiff(pts.b.ab, df1.mortality30.id) #414 patients

ptid.0ab.30_mortality <- intersect(pts.0ab, df1.mortality30.id) #205 patients 
ptid.0ab.30_survive <- setdiff(pts.0ab, df1.mortality30.id) #286 patients

length(ptid.b.ab.30_mortality) / length(pts.b.ab)
length(ptid.0ab.30_mortality) / length(pts.0ab)


#create dataframe for patients without antibiotics and if has 10% lactate clearance, censor = 1; else 0
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

table(lactateclearanceA$censor)

#create dataframe for patients with antibiotics and if has 10% lactate clearance, censor = 1; else 0
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
table(lactateclearanceB$censor)

# lactateclearanceA <- lactateclearanceA[order(lactateclearanceA$time),]
# boxplot(lactateclearanceA$time, main = 'boxplot of clearance time/censor time of patients without abx')
# 
# lactateclearanceB <- lactateclearanceB[order(lactateclearanceB$time),]
# boxplot(lactateclearanceB$time, main = 'boxplot of clearance time/censor time of patients with abx')
# 
# lactateclearanceA2 <- lactateclearanceA[(1:(nrow(lactateclearanceA)-1)),]
# boxplot(lactateclearanceA2$time, main = 'boxplot of clearance time/censor time of patients without abx')
# 
# lactateclearanceB2 <- lactateclearanceB[(1:(nrow(lactateclearanceB)-1)),]
# boxplot(lactateclearanceB2$time, main = 'boxplot of clearance time/censor time of patients with abx')
# 
# 
# lactateclearance.ab <- rbind(lactateclearanceA2, lactateclearanceB2)
# # lactateclearance.ab$drugab <- as.n(lactateclearance.ab$drugab)
# # lactateclearance.ab$censor <- as.factor(lactateclearance.ab$censor)
# # lactateclearance.ab$time <- as.numeric(lactateclearance.ab$time)
# str(lactateclearance.ab)

lactateclearance.ab <- rbind(lactateclearanceA, lactateclearanceB)


library(survival)
# lactateclearance.ab2 <- lactateclearance.ab
# str(lactateclearance.ab2)
# lactateclearance.ab2$survObj <- with(lactateclearance.ab2, Surv(time, censor==1))
# str(lactateclearance.ab2)

lactateclearance.ab2 <- lactateclearance.ab[(lactateclearance.ab$id != '25012#%#1#%#1'),]
# surv.as.one <- npsurv(survObj ~ 1, data = lactateclearance.ab2, conf.type = "log-log")
# 
# surv.by.numab <- npsurv(survObj ~ drugab, data = lactateclearance.ab2, conf.type = "log-log")


#survival analysis by abx given or not before clearance
survab <- survfit(Surv(time, censor) ~ drugab, data = lactateclearance.ab2)
print(survab)
plot(survab, lty=c(2, 1), col = c('red', 'green'), xlab="Time", ylab="Lactate Clearance Probability", fun = 'event')
legend(100, 0.2, c("abx = 0", "abx > 0"), lty=c(2, 1), col = c('red', 'green')) 


lactateclearanceA.abtime <- lactateclearanceA
lactateclearanceA.abtime$first.abtime <- 5000 # those patients do not have abx before lactate clearance but we gave it a large number to represent instead of using NA
lactateclearanceA.abtime$firstafterevent.abtime <- NA

#survival analysis by abx time before clearance
lactateclearanceB.abtime <- merge(lactateclearanceB, abtimes, by.x = 'id', by.y = 'id', all.x = T)
lactateclearanceB.abtime2 <- lactateclearanceB.abtime[(lactateclearanceB.abtime$id != '25012#%#1#%#1'),]
names(lactateclearanceA.abtime)
names(lactateclearanceB.abtime2)
lactateclearanceAB.abtime2 <- rbind(lactateclearanceA.abtime, lactateclearanceB.abtime2)


# firstab.time <- cut(lactateclearanceAB.abtime2$first.abtime, c(-1000, -6, 0, 6, 1000), right = F)
# survab.time <- survfit(Surv(time, censor) ~ firstab.time, data = lactateclearanceAB.abtime2)
# print(survab.time)
# plot(survab.time, lty=c(1,2,1,4), col = c('red', 'green', 'black', 'blue'), xlab="Time", ylab="Lactate Clearance Probability", fun = 'event')
# legend(200, 0.2, c('[-1000,-6)', '[-6,0)', '[0,6)', '[6,1000)' ), lty=c(1,2,1,4), col = c('red', 'green', 'black', 'blue')) 



firstab.time <- cut(lactateclearanceAB.abtime2$first.abtime, c(-2000, -12, -6, 0, 3, 6, 1000, 10000), right = F)
survab.time <- survfit(Surv(time, censor) ~ firstab.time, data = lactateclearanceAB.abtime2)
print(survab.time)
plot(survab.time, lty=c(1,1,1,1,5,6,6), col = c('red', 'green', 'black', 'blue','orange', 'purple', 'brown'), xlab="Time", ylab="Lactate Clearance Probability", fun = 'event')
legend(200, 0.5, c('[-2000,-12)', '[-12, -6)', '[-6,0)', '[0,3)','[3,6)', '[6,1000)', 'No ABX' ), lty=c(1,1,1,1,5,6), col = c('red', 'green', 'black', 'blue','orange', 'purple', 'brown')) 


names(lactateclearanceB.abtime2)
names(lactateclearanceA)

pts.b.ab1 <- lactateclearanceAB.abtime2$id[lactateclearanceAB.abtime2$first.abtime > -1000 & lactateclearanceAB.abtime2$first.abtime < -12 ]
pts.b.ab2 <- lactateclearanceAB.abtime2$id[lactateclearanceAB.abtime2$first.abtime >= -12 & lactateclearanceAB.abtime2$first.abtime < -6]
pts.b.ab3 <- lactateclearanceAB.abtime2$id[lactateclearanceAB.abtime2$first.abtime >= -6 & lactateclearanceAB.abtime2$first.abtime < 0]
pts.b.ab4 <- lactateclearanceAB.abtime2$id[lactateclearanceAB.abtime2$first.abtime >= 0 & lactateclearanceAB.abtime2$first.abtime < 3]
pts.b.ab5 <- lactateclearanceAB.abtime2$id[lactateclearanceAB.abtime2$first.abtime >= 3 & lactateclearanceAB.abtime2$first.abtime < 6]
pts.b.ab6 <- lactateclearanceAB.abtime2$id[lactateclearanceAB.abtime2$first.abtime >= 6 & lactateclearanceAB.abtime2$first.abtime < 1000]
pts.b.ab0 <- lactateclearanceAB.abtime2$id[lactateclearanceAB.abtime2$first.abtime == 5000]

#mortality among patients who had or did not have antibiotics before lactate clearance by 10%
ptid.b.ab1.icu_mortality <- intersect(pts.b.ab1, ptid.icu_mortality)
ptid.b.ab2.icu_mortality <- intersect(pts.b.ab2, ptid.icu_mortality)
ptid.b.ab3.icu_mortality <- intersect(pts.b.ab3, ptid.icu_mortality) 
ptid.b.ab4.icu_mortality <- intersect(pts.b.ab4, ptid.icu_mortality) 
ptid.b.ab5.icu_mortality <- intersect(pts.b.ab5, ptid.icu_mortality)
ptid.b.ab6.icu_mortality <- intersect(pts.b.ab6, ptid.icu_mortality) 
ptid.b.ab0.icu_mortality <- intersect(pts.b.ab0, ptid.icu_mortality) 

length(ptid.b.ab1.icu_mortality) / length(pts.b.ab1)
length(ptid.b.ab2.icu_mortality) / length(pts.b.ab2)
length(ptid.b.ab3.icu_mortality) / length(pts.b.ab3)
length(ptid.b.ab4.icu_mortality) / length(pts.b.ab4)
length(ptid.b.ab5.icu_mortality) / length(pts.b.ab5)
length(ptid.b.ab6.icu_mortality) / length(pts.b.ab6)
length(ptid.b.ab0.icu_mortality) / length(pts.b.ab0)


ptid.b.ab1.hosp_mortality <- intersect(pts.b.ab1, ptid.hosp_mortality)
ptid.b.ab2.hosp_mortality <- intersect(pts.b.ab2, ptid.hosp_mortality)
ptid.b.ab3.hosp_mortality <- intersect(pts.b.ab3, ptid.hosp_mortality) 
ptid.b.ab4.hosp_mortality <- intersect(pts.b.ab4, ptid.hosp_mortality) 
ptid.b.ab5.hosp_mortality <- intersect(pts.b.ab5, ptid.hosp_mortality)
ptid.b.ab6.hosp_mortality <- intersect(pts.b.ab6, ptid.hosp_mortality) 
ptid.b.ab0.hosp_mortality <- intersect(pts.b.ab0, ptid.hosp_mortality) 
library(stringr)
hospids <- function(ids) {
  ids2 <- lapply(ids, function (x) str_sub(x, 1, -5))
  ids2 <- unique(unlist(ids2))
  return(ids2)
}


pts.b.ab1.id2 <- hospids(pts.b.ab1)
pts.b.ab2.id2 <- hospids(pts.b.ab2)
pts.b.ab3.id2 <- hospids(pts.b.ab3)
pts.b.ab4.id2 <- hospids(pts.b.ab4)
pts.b.ab5.id2 <- hospids(pts.b.ab5)
pts.b.ab6.id2 <- hospids(pts.b.ab6)
pts.b.ab0.id2 <- hospids(pts.b.ab0)

length(ptid.b.ab1.hosp_mortality) / length(pts.b.ab1.id2)
length(ptid.b.ab2.hosp_mortality) / length(pts.b.ab2.id2)
length(ptid.b.ab3.hosp_mortality) / length(pts.b.ab3.id2)
length(ptid.b.ab4.hosp_mortality) / length(pts.b.ab4.id2)
length(ptid.b.ab5.hosp_mortality) / length(pts.b.ab5.id2)
length(ptid.b.ab6.hosp_mortality) / length(pts.b.ab6.id2)
length(ptid.b.ab0.hosp_mortality) / length(pts.b.ab0.id2)


ptid.b.ab1.30_mortality <- intersect(pts.b.ab1, df1.mortality30.id) 
ptid.b.ab2.30_mortality <- intersect(pts.b.ab2, df1.mortality30.id) 
ptid.b.ab3.30_mortality <- intersect(pts.b.ab3, df1.mortality30.id) 
ptid.b.ab4.30_mortality <- intersect(pts.b.ab4, df1.mortality30.id) 
ptid.b.ab5.30_mortality <- intersect(pts.b.ab5, df1.mortality30.id) 
ptid.b.ab6.30_mortality <- intersect(pts.b.ab6, df1.mortality30.id) 
ptid.b.ab0.30_mortality <- intersect(pts.b.ab0, df1.mortality30.id) 



length(ptid.b.ab1.30_mortality) / length(pts.b.ab1)
length(ptid.b.ab2.30_mortality) / length(pts.b.ab2)
length(ptid.b.ab3.30_mortality) / length(pts.b.ab3)
length(ptid.b.ab4.30_mortality) / length(pts.b.ab4)
length(ptid.b.ab5.30_mortality) / length(pts.b.ab5)
length(ptid.b.ab6.30_mortality) / length(pts.b.ab6)
length(ptid.b.ab0.30_mortality) / length(pts.b.ab0)

# library(rms)
# survplot(surv.as.one, xlab = 'hours', ylab = 'lactate no-clearance probability')
# survplot(surv.by.numab, xlab = 'hours', ylab = 'lactate no-clearance probability')
# 
# survdiff(Surv(time, censor) ~ drugab, data=lactateclearance.ab, rho=0)


# Density plots with means for the initial lacatate in two groups
lactateclearance.ab2$drugab2 <- as.factor(lactateclearance.ab2$drugab)
#mean initial lactate (drugab==0): 6.86; mean(drugab==1): 6.31
summary(lactateclearanceA$initial.lactate)
summary(lactateclearanceB$initial.lactate)


ggplot(lactateclearance.ab2, aes(x=initial.lactate, colour=drugab2)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=drugab2),
             linetype="dashed", size=1) + xlim(0, 20)

# Density plots with means for the clearance time
lactateclearance.ab3 <- lactateclearance.ab2[lactateclearance.ab2$censor == 1,]

lactateclearance.ab3$drugab2 <- as.factor(lactateclearance.ab3$drugab)
cdat2 <- ddply(lactateclearance.ab3, "drugab2", summarise, rating.mean=mean(time))
#mean clearance timing (do not consider censored patients) (drugab==0): 8.44; mean(drugab==1): 13.75
summary(lactateclearance.ab3$time[lactateclearance.ab3$drugab == 0])
summary(lactateclearance.ab3$time[lactateclearance.ab3$drugab == 1])

ggplot(lactateclearance.ab3, aes(x=time, colour=drugab2)) +
  geom_density() +
  geom_vline(data=cdat2, aes(xintercept=rating.mean,  colour=drugab2),
             linetype="dashed", size=1) + xlim(0, 100)


# SOFA and SAPS1 scores
ptinfos2 <- read.csv('pticu_infos2.csv', header = T)
ptinfos2$id <- paste(ptinfos2$subject_id, ptinfos2$hospital_seq, ptinfos2$icustay_seq, sep='#%#')
names(ptinfos2)


pts.b.ab1.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab1, c(19, 10:15)]
pts.b.ab1.scores$ab <- 1
pts.b.ab2.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab2, c(19, 10:15)]
pts.b.ab2.scores$ab <- 2
pts.b.ab3.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab3, c(19, 10:15)]
pts.b.ab3.scores$ab <- 3
pts.b.ab4.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab4, c(19, 10:15)]
pts.b.ab4.scores$ab <- 4
pts.b.ab5.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab5, c(19, 10:15)]
pts.b.ab5.scores$ab <- 5
pts.b.ab6.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab6, c(19, 10:15)]
pts.b.ab6.scores$ab <- 6
pts.b.ab0.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab0, c(19, 10:15)]
pts.b.ab0.scores$ab <- 7

pts.b.ab.scores <- rbind(pts.b.ab1.scores, pts.b.ab2.scores)
pts.b.ab.scores <- rbind(pts.b.ab.scores, pts.b.ab3.scores)
pts.b.ab.scores <- rbind(pts.b.ab.scores, pts.b.ab4.scores)
pts.b.ab.scores <- rbind(pts.b.ab.scores, pts.b.ab5.scores)
pts.b.ab.scores <- rbind(pts.b.ab.scores, pts.b.ab6.scores)
pts.b.ab.scores <- rbind(pts.b.ab.scores, pts.b.ab0.scores)


pts.b.ab.scores$ab2 <- as.factor(pts.b.ab.scores$ab)
str(pts.b.ab.scores)
cdat <- ddply(pts.b.ab.scores, "ab2", summarise, rating.mean=mean(sapsi_max, na.rm = T)) 
ggplot(pts.b.ab.scores, aes(x=sapsi_max, colour=ab2)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=ab2),
             linetype="dashed", size=1)

cdat <- ddply(pts.b.ab.scores, "ab2", summarise, rating.mean=mean(sapsi_first, na.rm = T)) 
ggplot(pts.b.ab.scores, aes(x=sapsi_first, colour=ab2)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=ab2),
             linetype="dashed", size=1)

cdat <- ddply(pts.b.ab.scores, "ab2", summarise, rating.mean=mean(sofa_max, na.rm = T)) 
ggplot(pts.b.ab.scores, aes(x=sofa_max, colour=ab2)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=ab2),
             linetype="dashed", size=1)

cdat <- ddply(pts.b.ab.scores, "ab2", summarise, rating.mean=mean(sofa_first, na.rm = T)) 
ggplot(pts.b.ab.scores, aes(x=sofa_first, colour=ab2)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=ab2),
             linetype="dashed", size=1)









#================================================= lactate normalization===========================================================================================
#remove the antibiotics given after lactate normalization
antibiotics5a <- antibiotics5[(is.na(antibiotics5$normalizetime)) | ((!is.na(antibiotics5$normalizetime)) & (antibiotics5$dftime < antibiotics5$normalizetime)), ]
antibiotics5a <- antibiotics5a[!is.na(antibiotics5a$id),]
length(unique(antibiotics5a$id))

#aggregate to counts by ptid by antibiotic category
antibiotics5a.count <- count(antibiotics5a, c('id', 'itemidG'))
str(antibiotics5a.count)
unique(antibiotics5a.count$id)

#further aggregate to get patients with antibiotics
antibiotics5a.count.noNA <- antibiotics5a.count[!is.na(antibiotics5a.count$itemidG),-3]
antibiotics5a.numab.count <- count(antibiotics5a.count.noNA, 'id')
length(unique(antibiotics5a.numab.count$id)) 


#patients with antibiotics before lactate normalization
pts.ab <- unique(antibiotics5a.numab.count$id) #
#patients without any antibiotics before lactate normalization
pts.0ab <- setdiff(severe.sepsis2.eventtime.3$id, pts.ab) #



#keep patients the antibiotics given after sepsis and before lactate normalization
antibiotics5b <- antibiotics5[(is.na(antibiotics5$normalizetime)) | ((!is.na(antibiotics5$normalizetime)) & (antibiotics5$dftime < antibiotics5$normalizetime) & (antibiotics5$dftime >= 0)), ]
antibiotics5b <- antibiotics5b[!is.na(antibiotics5b$id),]
length(unique(antibiotics5b$id))
 

#aggregate to counts by ptid by antibiotic category
antibiotics5b.count <- count(antibiotics5b, c('id', 'itemidG'))
str(antibiotics5b.count)
unique(antibiotics5b.count$id)

#further aggregate to get patients with antibiotics
antibiotics5b.count.noNA <- antibiotics5b.count[!is.na(antibiotics5b.count$itemidG),-3]
antibiotics5b.numab.count <- count(antibiotics5b.count.noNA, 'id')
length(unique(antibiotics5b.numab.count$id)) #888


#patients with antibiotics before lactate normalization and after developed severe sepsis
pts.b.ab <- unique(antibiotics5b.numab.count$id) 
#patients without any antibiotics lactate normalization
pts.b.0ab <- pts.0ab


library(stringr)
hospids <- function(ids) {
  ids2 <- lapply(ids, function (x) str_sub(x, 1, -5))
  ids2 <- unique(unlist(ids2))
  return(ids2)
}


#create dataframe for patients without antibiotics and if has lactate normalization, censor = 1; else 0
str(lactateclearance)
lactateclearanceA <- lactateclearance[lactateclearance$id %in% pts.0ab, c(1,2,4,5)]

lactateclearanceA$censor <- NA
lactateclearanceA$time <- NA
for (i in 1:nrow(lactateclearanceA)) {
  if (is.na(lactateclearanceA$normalizetime[i])) {
    lactateclearanceA$time[i] <- lactateclearanceA$lastlactate.dftime[i]
    lactateclearanceA$censor[i] <- 0
  }  else {
    lactateclearanceA$time[i] <- lactateclearanceA$normalizetime[i]
    lactateclearanceA$censor[i] <- 1 
  }
}
lactateclearanceA$drugab <- 0

table(lactateclearanceA$censor)

#create dataframe for patients with antibiotics and if has 10% lactate clearance, censor = 1; else 0
lactateclearanceB <- lactateclearance[lactateclearance$id %in% pts.ab, c(1,2,4,5)]

lactateclearanceB$censor <- NA
lactateclearanceB$time <- NA
for (i in 1:nrow(lactateclearanceB)) {
  if (is.na(lactateclearanceB$normalizetime[i])) {
    lactateclearanceB$time[i] <- lactateclearanceB$lastlactate.dftime[i]
    lactateclearanceB$censor[i] <- 0
  }  else {
    lactateclearanceB$time[i] <- lactateclearanceB$normalizetime[i]
    lactateclearanceB$censor[i] <- 1   
  }
}
lactateclearanceB$drugab <- 1
table(lactateclearanceB$censor)

lactateclearance.ab <- rbind(lactateclearanceA, lactateclearanceB)


library(survival)
# lactateclearance.ab2 <- lactateclearance.ab
# str(lactateclearance.ab2)
# lactateclearance.ab2$survObj <- with(lactateclearance.ab2, Surv(time, censor==1))
# str(lactateclearance.ab2)

lactateclearance.ab2 <- lactateclearance.ab[(lactateclearance.ab$time < 2000),]
# surv.as.one <- npsurv(survObj ~ 1, data = lactateclearance.ab2, conf.type = "log-log")
# 
# surv.by.numab <- npsurv(survObj ~ drugab, data = lactateclearance.ab2, conf.type = "log-log")


#survival analysis by abx given or not before clearance
# survab <- survfit(Surv(time, censor) ~ drugab, data = lactateclearance.ab2)
# print(survab)
# plot(survab, lty=c(2, 1), col = c('red', 'green'), xlab="Time", ylab="Lactate Normalization Probability", fun = 'event')
# legend(100, 0.2, c("abx = 0", "abx > 0"), lty=c(2, 1), col = c('red', 'green')) 


lactateclearanceA.abtime <- lactateclearanceA[lactateclearanceA$time < 900,]
lactateclearanceA.abtime$first.abtime <- 5000 # those patients do not have abx before lactate clearance but we gave it a large number to represent instead of using NA
lactateclearanceA.abtime$firstafterevent.abtime <- NA

#survival analysis by abx time before clearance
lactateclearanceB.abtime <- merge(lactateclearanceB, abtimes, by.x = 'id', by.y = 'id', all.x = T)
lactateclearanceB.abtime2 <- lactateclearanceB.abtime[(lactateclearanceB.abtime$time < 900),]
names(lactateclearanceA.abtime)
names(lactateclearanceB.abtime2)
lactateclearanceAB.abtime2 <- rbind(lactateclearanceA.abtime, lactateclearanceB.abtime2)


# firstab.time <- cut(lactateclearanceAB.abtime2$first.abtime, c(-1000, -6, 0, 6, 1000), right = F)
# survab.time <- survfit(Surv(time, censor) ~ firstab.time, data = lactateclearanceAB.abtime2)
# print(survab.time)
# plot(survab.time, lty=c(1,2,1,4), col = c('red', 'green', 'black', 'blue'), xlab="Time", ylab="Lactate Clearance Probability", fun = 'event')
# legend(200, 0.2, c('[-1000,-6)', '[-6,0)', '[0,6)', '[6,1000)' ), lty=c(1,2,1,4), col = c('red', 'green', 'black', 'blue')) 


summary(lactateclearanceAB.abtime2$time)
firstab.time <- cut(lactateclearanceAB.abtime2$first.abtime, c(-2000, -12, -6, 0, 3, 6, 1000, 10000), right = F)
survab.time <- survfit(Surv(time, censor) ~ firstab.time, data = lactateclearanceAB.abtime2)
print(survab.time)
plot(survab.time, lty=c(1,1,1,1,5,6,6), col = c('red', 'green', 'black', 'blue','orange', 'purple', 'brown'), xlab="Time", ylab="Lactate Normalization Probability", fun = 'event')
legend(200, 0.5, c('[-2000,-12)', '[-12, -6)', '[-6,0)', '[0,3)','[3,6)', '[6,1000)', 'No ABX' ), lty=c(1,1,1,1,5,6), col = c('red', 'green', 'black', 'blue','orange', 'purple', 'brown')) 


dim(lactateclearanceB.abtime2)
dim(lactateclearanceA)

pts.b.ab1 <- lactateclearanceAB.abtime2$id[lactateclearanceAB.abtime2$first.abtime > -2000 & lactateclearanceAB.abtime2$first.abtime < -12 ]
pts.b.ab2 <- lactateclearanceAB.abtime2$id[lactateclearanceAB.abtime2$first.abtime >= -12 & lactateclearanceAB.abtime2$first.abtime < -6]
pts.b.ab3 <- lactateclearanceAB.abtime2$id[lactateclearanceAB.abtime2$first.abtime >= -6 & lactateclearanceAB.abtime2$first.abtime < 0]
pts.b.ab4 <- lactateclearanceAB.abtime2$id[lactateclearanceAB.abtime2$first.abtime >= 0 & lactateclearanceAB.abtime2$first.abtime < 3]
pts.b.ab5 <- lactateclearanceAB.abtime2$id[lactateclearanceAB.abtime2$first.abtime >= 3 & lactateclearanceAB.abtime2$first.abtime < 6]
pts.b.ab6 <- lactateclearanceAB.abtime2$id[lactateclearanceAB.abtime2$first.abtime >= 6 & lactateclearanceAB.abtime2$first.abtime < 1000]
pts.b.ab0 <- lactateclearanceAB.abtime2$id[lactateclearanceAB.abtime2$first.abtime == 5000]

#mortality among patients who had or did not have antibiotics before lactate clearance by 10%
ptid.b.ab1.icu_mortality <- intersect(pts.b.ab1, ptid.icu_mortality)
ptid.b.ab2.icu_mortality <- intersect(pts.b.ab2, ptid.icu_mortality)
ptid.b.ab3.icu_mortality <- intersect(pts.b.ab3, ptid.icu_mortality) 
ptid.b.ab4.icu_mortality <- intersect(pts.b.ab4, ptid.icu_mortality) 
ptid.b.ab5.icu_mortality <- intersect(pts.b.ab5, ptid.icu_mortality)
ptid.b.ab6.icu_mortality <- intersect(pts.b.ab6, ptid.icu_mortality) 
ptid.b.ab0.icu_mortality <- intersect(pts.b.ab0, ptid.icu_mortality) 

length(ptid.b.ab1.icu_mortality) / length(pts.b.ab1)
length(ptid.b.ab2.icu_mortality) / length(pts.b.ab2)
length(ptid.b.ab3.icu_mortality) / length(pts.b.ab3)
length(ptid.b.ab4.icu_mortality) / length(pts.b.ab4)
length(ptid.b.ab5.icu_mortality) / length(pts.b.ab5)
length(ptid.b.ab6.icu_mortality) / length(pts.b.ab6)
length(ptid.b.ab0.icu_mortality) / length(pts.b.ab0)


ptid.b.ab1.hosp_mortality <- intersect(pts.b.ab1, ptid.hosp_mortality)
ptid.b.ab2.hosp_mortality <- intersect(pts.b.ab2, ptid.hosp_mortality)
ptid.b.ab3.hosp_mortality <- intersect(pts.b.ab3, ptid.hosp_mortality) 
ptid.b.ab4.hosp_mortality <- intersect(pts.b.ab4, ptid.hosp_mortality) 
ptid.b.ab5.hosp_mortality <- intersect(pts.b.ab5, ptid.hosp_mortality)
ptid.b.ab6.hosp_mortality <- intersect(pts.b.ab6, ptid.hosp_mortality) 
ptid.b.ab0.hosp_mortality <- intersect(pts.b.ab0, ptid.hosp_mortality) 
library(stringr)
hospids <- function(ids) {
  ids2 <- lapply(ids, function (x) str_sub(x, 1, -5))
  ids2 <- unique(unlist(ids2))
  return(ids2)
}


pts.b.ab1.id2 <- hospids(pts.b.ab1)
pts.b.ab2.id2 <- hospids(pts.b.ab2)
pts.b.ab3.id2 <- hospids(pts.b.ab3)
pts.b.ab4.id2 <- hospids(pts.b.ab4)
pts.b.ab5.id2 <- hospids(pts.b.ab5)
pts.b.ab6.id2 <- hospids(pts.b.ab6)
pts.b.ab0.id2 <- hospids(pts.b.ab0)

length(ptid.b.ab1.hosp_mortality) / length(pts.b.ab1.id2)
length(ptid.b.ab2.hosp_mortality) / length(pts.b.ab2.id2)
length(ptid.b.ab3.hosp_mortality) / length(pts.b.ab3.id2)
length(ptid.b.ab4.hosp_mortality) / length(pts.b.ab4.id2)
length(ptid.b.ab5.hosp_mortality) / length(pts.b.ab5.id2)
length(ptid.b.ab6.hosp_mortality) / length(pts.b.ab6.id2)
length(ptid.b.ab0.hosp_mortality) / length(pts.b.ab0.id2)


ptid.b.ab1.30_mortality <- intersect(pts.b.ab1, df1.mortality30.id) 
ptid.b.ab2.30_mortality <- intersect(pts.b.ab2, df1.mortality30.id) 
ptid.b.ab3.30_mortality <- intersect(pts.b.ab3, df1.mortality30.id) 
ptid.b.ab4.30_mortality <- intersect(pts.b.ab4, df1.mortality30.id) 
ptid.b.ab5.30_mortality <- intersect(pts.b.ab5, df1.mortality30.id) 
ptid.b.ab6.30_mortality <- intersect(pts.b.ab6, df1.mortality30.id) 
ptid.b.ab0.30_mortality <- intersect(pts.b.ab0, df1.mortality30.id) 



length(ptid.b.ab1.30_mortality) / length(pts.b.ab1)
length(ptid.b.ab2.30_mortality) / length(pts.b.ab2)
length(ptid.b.ab3.30_mortality) / length(pts.b.ab3)
length(ptid.b.ab4.30_mortality) / length(pts.b.ab4)
length(ptid.b.ab5.30_mortality) / length(pts.b.ab5)
length(ptid.b.ab6.30_mortality) / length(pts.b.ab6)
length(ptid.b.ab0.30_mortality) / length(pts.b.ab0)

# library(rms)
# survplot(surv.as.one, xlab = 'hours', ylab = 'lactate no-clearance probability')
# survplot(surv.by.numab, xlab = 'hours', ylab = 'lactate no-clearance probability')
# 
# survdiff(Surv(time, censor) ~ drugab, data=lactateclearance.ab, rho=0)


# Density plots with means for the initial lacatate in two groups
lactateclearance.ab2$drugab2 <- as.factor(lactateclearance.ab2$drugab)
cdat <- ddply(lactateclearance.ab2, "drugab2", summarise, rating.mean=mean(initial.lactate)) 
#mean initial lactate (drugab==0): 6.86; mean(drugab==1): 6.31
summary(lactateclearanceA$initial.lactate)
summary(lactateclearanceB$initial.lactate)


ggplot(lactateclearance.ab2, aes(x=initial.lactate, colour=drugab2)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=drugab2),
             linetype="dashed", size=1) + xlim(0, 20)

# Density plots with means for the clearance time
lactateclearance.ab3 <- lactateclearance.ab2[lactateclearance.ab2$censor == 1,]

lactateclearance.ab3$drugab2 <- as.factor(lactateclearance.ab3$drugab)
cdat2 <- ddply(lactateclearance.ab3, "drugab2", summarise, rating.mean=mean(time))
#mean clearance timing (do not consider censored patients) (drugab==0): 8.44; mean(drugab==1): 13.75
summary(lactateclearance.ab3$time[lactateclearance.ab3$drugab == 0])
summary(lactateclearance.ab3$time[lactateclearance.ab3$drugab == 1])

ggplot(lactateclearance.ab3, aes(x=time, colour=drugab2)) +
  geom_density() +
  geom_vline(data=cdat2, aes(xintercept=rating.mean,  colour=drugab2),
             linetype="dashed", size=1) + xlim(0, 100)


# SOFA and SAPS1 scores
ptinfos2 <- read.csv('pticu_infos2.csv', header = T)
ptinfos2$id <- paste(ptinfos2$subject_id, ptinfos2$hospital_seq, ptinfos2$icustay_seq, sep='#%#')
names(ptinfos2)


pts.b.ab1.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab1, c(19, 10:15)]
pts.b.ab1.scores$ab <- 1
pts.b.ab2.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab2, c(19, 10:15)]
pts.b.ab2.scores$ab <- 2
pts.b.ab3.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab3, c(19, 10:15)]
pts.b.ab3.scores$ab <- 3
pts.b.ab4.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab4, c(19, 10:15)]
pts.b.ab4.scores$ab <- 4
pts.b.ab5.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab5, c(19, 10:15)]
pts.b.ab5.scores$ab <- 5
pts.b.ab6.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab6, c(19, 10:15)]
pts.b.ab6.scores$ab <- 6
pts.b.ab0.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab0, c(19, 10:15)]
pts.b.ab0.scores$ab <- 7

pts.b.ab.scores <- rbind(pts.b.ab1.scores, pts.b.ab2.scores)
pts.b.ab.scores <- rbind(pts.b.ab.scores, pts.b.ab3.scores)
pts.b.ab.scores <- rbind(pts.b.ab.scores, pts.b.ab4.scores)
pts.b.ab.scores <- rbind(pts.b.ab.scores, pts.b.ab5.scores)
pts.b.ab.scores <- rbind(pts.b.ab.scores, pts.b.ab6.scores)
pts.b.ab.scores <- rbind(pts.b.ab.scores, pts.b.ab0.scores)


pts.b.ab.scores$ab2 <- as.factor(pts.b.ab.scores$ab)
str(pts.b.ab.scores)
cdat <- ddply(pts.b.ab.scores, "ab2", summarise, rating.mean=mean(sapsi_max, na.rm = T)) 
ggplot(pts.b.ab.scores, aes(x=sapsi_max, colour=ab2)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=ab2),
             linetype="dashed", size=1)

cdat <- ddply(pts.b.ab.scores, "ab2", summarise, rating.mean=mean(sapsi_first, na.rm = T)) 
ggplot(pts.b.ab.scores, aes(x=sapsi_first, colour=ab2)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=ab2),
             linetype="dashed", size=1)

cdat <- ddply(pts.b.ab.scores, "ab2", summarise, rating.mean=mean(sofa_max, na.rm = T)) 
ggplot(pts.b.ab.scores, aes(x=sofa_max, colour=ab2)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=ab2),
             linetype="dashed", size=1)

cdat <- ddply(pts.b.ab.scores, "ab2", summarise, rating.mean=mean(sofa_first, na.rm = T)) 
ggplot(pts.b.ab.scores, aes(x=sofa_first, colour=ab2)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=ab2),
             linetype="dashed", size=1)


#=================================HIV patients =========================================================

HIVicd9 <- read.csv('HIVicd9.csv', header = T)
HIVicd9$id <- paste(HIVicd9$subject_id, HIVicd9$hospital_seq, HIVicd9$icustay_seq, sep='#%#')
HIVicd9$id0 <- paste(HIVicd9$subject_id, HIVicd9$hospital_seq, sep='#%#')
HIVicd9.id <- unique(HIVicd9$id) #404
HIVicd9.id0 <- unique(HIVicd9$id0) #376
HIVicd9.id00 <- unique(HIVicd9$subject_id) #308
HIVtests <- read.csv('HIVtests.csv', header = T)
HIVtests$id <- paste(HIVtests$subject_id, HIVtests$hospital_seq, HIVtests$icustay_seq, sep='#%#')
HIVtestsCD4 <- HIVtests[HIVtests$itemid==50356 & (HIVtests$valuenum > 15 | HIVtests$valuenum < 6), ]
HIVtestsCD4.id <- unique(HIVtestsCD4$id) #96
hist(HIVtestsCD4$valuenum)

#================================patients who are with severe sepsis and HIV+/HIV-==================================

lactateclearanceAB.abtime2$hiv <- 0
lactateclearanceAB.abtime2$hiv[lactateclearanceAB.abtime2$id %in% HIVicd9.id] <- 1


# firstab.time <- cut(lactateclearanceAB.abtime2$first.abtime, c(-1000, -6, 0, 6, 1000), right = F)
# survab.time <- survfit(Surv(time, censor) ~ firstab.time, data = lactateclearanceAB.abtime2)
# print(survab.time)
# plot(survab.time, lty=c(1,2,1,4), col = c('red', 'green', 'black', 'blue'), xlab="Time", ylab="Lactate Clearance Probability", fun = 'event')
# legend(200, 0.2, c('[-1000,-6)', '[-6,0)', '[0,6)', '[6,1000)' ), lty=c(1,2,1,4), col = c('red', 'green', 'black', 'blue')) 


survab.hiv <- survfit(Surv(time, censor) ~ hiv, data = lactateclearanceAB.abtime2)
print(survab.hiv)
plot(survab.hiv, lty=c(1,1), col = c('red', 'black'), xlab="Time", ylab="Lactate Normalization Probability", fun = 'event')
legend(200, 0.5, c('HIV-', 'HIV+' ), lty=c(1,1), col = c('red', 'black')) 


pts.b.ab0 <- lactateclearanceAB.abtime2$id[lactateclearanceAB.abtime2$hiv == 0 ]
pts.b.ab1 <- lactateclearanceAB.abtime2$id[lactateclearanceAB.abtime2$hiv == 1]

#mortality among patients who had or did not have antibiotics before lactate clearance by 10%
ptid.b.ab0.icu_mortality <- intersect(pts.b.ab0, ptid.icu_mortality)
ptid.b.ab1.icu_mortality <- intersect(pts.b.ab1, ptid.icu_mortality)

length(ptid.b.ab0.icu_mortality) / length(pts.b.ab0)
length(ptid.b.ab1.icu_mortality) / length(pts.b.ab1)

ptid.b.ab0.hosp_mortality <- intersect(pts.b.ab0, ptid.hosp_mortality)
ptid.b.ab1.hosp_mortality <- intersect(pts.b.ab1, ptid.hosp_mortality)


pts.b.ab0.id2 <- hospids(pts.b.ab0)
pts.b.ab1.id2 <- hospids(pts.b.ab1)

length(ptid.b.ab0.hosp_mortality) / length(pts.b.ab0.id2)
length(ptid.b.ab1.hosp_mortality) / length(pts.b.ab1.id2)

ptid.b.ab0.30_mortality <- intersect(pts.b.ab0, df1.mortality30.id) 
ptid.b.ab1.30_mortality <- intersect(pts.b.ab1, df1.mortality30.id) 

length(ptid.b.ab0.30_mortality) / length(pts.b.ab0)
length(ptid.b.ab1.30_mortality) / length(pts.b.ab1)

pts.b.ab0.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab0, c(19, 10:15)]
pts.b.ab0.scores$ab <- 0
pts.b.ab1.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab1, c(19, 10:15)]
pts.b.ab1.scores$ab <- 1


pts.b.ab.scores <- rbind(pts.b.ab0.scores, pts.b.ab1.scores)



pts.b.ab.scores$ab2 <- as.factor(pts.b.ab.scores$ab)
str(pts.b.ab.scores)
library(ggplot2)
cdat <- ddply(pts.b.ab.scores, "ab2", summarise, rating.mean=mean(sapsi_max, na.rm = T)) 
p1 <- ggplot(pts.b.ab.scores, aes(x=sapsi_max, colour=ab2)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=ab2),
             linetype="dashed", size=1)

cdat <- ddply(pts.b.ab.scores, "ab2", summarise, rating.mean=mean(sapsi_first, na.rm = T)) 
p2 <- ggplot(pts.b.ab.scores, aes(x=sapsi_first, colour=ab2)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=ab2),
             linetype="dashed", size=1)

cdat <- ddply(pts.b.ab.scores, "ab2", summarise, rating.mean=mean(sofa_max, na.rm = T)) 
p3 <- ggplot(pts.b.ab.scores, aes(x=sofa_max, colour=ab2)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=ab2),
             linetype="dashed", size=1)

cdat <- ddply(pts.b.ab.scores, "ab2", summarise, rating.mean=mean(sofa_first, na.rm = T)) 
p4 <- ggplot(pts.b.ab.scores, aes(x=sofa_first, colour=ab2)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=ab2),
             linetype="dashed", size=1)

multiplot(p1, p2, p3, p4, cols=2)




# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# firstab.time <- cut(lactateclearanceAB.abtime2$first.abtime, c(-1000, -6, 0, 6, 1000), right = F)
# survab.time <- survfit(Surv(time, censor) ~ firstab.time, data = lactateclearanceAB.abtime2)
# print(survab.time)
# plot(survab.time, lty=c(1,2,1,4), col = c('red', 'green', 'black', 'blue'), xlab="Time", ylab="Lactate Clearance Probability", fun = 'event')
# legend(200, 0.2, c('[-1000,-6)', '[-6,0)', '[0,6)', '[6,1000)' ), lty=c(1,2,1,4), col = c('red', 'green', 'black', 'blue')) 


#survival analysis by first abx time 
lactateclearanceAB.abtime2.hiv <- lactateclearanceAB.abtime2[lactateclearanceAB.abtime2$id %in% HIVicd9.id,]

# firstab.time <- cut(lactateclearanceAB.abtime2$first.abtime, c(-1000, -6, 0, 6, 1000), right = F)
# survab.time <- survfit(Surv(time, censor) ~ firstab.time, data = lactateclearanceAB.abtime2)
# print(survab.time)
# plot(survab.time, lty=c(1,2,1,4), col = c('red', 'green', 'black', 'blue'), xlab="Time", ylab="Lactate Clearance Probability", fun = 'event')
# legend(200, 0.2, c('[-1000,-6)', '[-6,0)', '[0,6)', '[6,1000)' ), lty=c(1,2,1,4), col = c('red', 'green', 'black', 'blue')) 

summary(lactateclearanceAB.abtime2.hiv$time)
firstab.time <- cut(lactateclearanceAB.abtime2.hiv$first.abtime, c(-2000, 0, 1000, 10000), right = F)
survab.time <- survfit(Surv(time, censor) ~ firstab.time, data = lactateclearanceAB.abtime2.hiv)
print(survab.time)
plot(survab.time, lty=c(1,1,1), col = c('red', 'green', 'black'), xlab="Time", ylab="Lactate Normalization Probability", fun = 'event')
legend(200, 0.4, c('Before event', 'after event', 'No ABX' ), lty=c(1,1,1), col = c('red', 'green', 'black')) 


pts.b.ab1 <- lactateclearanceAB.abtime2.hiv$id[lactateclearanceAB.abtime2.hiv$first.abtime > -2000 & lactateclearanceAB.abtime2.hiv$first.abtime < 0 ]
pts.b.ab2 <- lactateclearanceAB.abtime2.hiv$id[lactateclearanceAB.abtime2.hiv$first.abtime >= 0 & lactateclearanceAB.abtime2.hiv$first.abtime < 1000]
pts.b.ab0 <- lactateclearanceAB.abtime2.hiv$id[lactateclearanceAB.abtime2.hiv$first.abtime == 5000]

#mortality among patients who had or did not have antibiotics before lactate clearance by 10%
ptid.b.ab1.icu_mortality <- intersect(pts.b.ab1, ptid.icu_mortality)
ptid.b.ab2.icu_mortality <- intersect(pts.b.ab2, ptid.icu_mortality)
ptid.b.ab0.icu_mortality <- intersect(pts.b.ab0, ptid.icu_mortality) 

length(ptid.b.ab1.icu_mortality) / length(pts.b.ab1)
length(ptid.b.ab2.icu_mortality) / length(pts.b.ab2)
length(ptid.b.ab0.icu_mortality) / length(pts.b.ab0)


ptid.b.ab1.hosp_mortality <- intersect(pts.b.ab1, ptid.hosp_mortality)
ptid.b.ab2.hosp_mortality <- intersect(pts.b.ab2, ptid.hosp_mortality)
ptid.b.ab0.hosp_mortality <- intersect(pts.b.ab0, ptid.hosp_mortality) 
library(stringr)
hospids <- function(ids) {
  ids2 <- lapply(ids, function (x) str_sub(x, 1, -5))
  ids2 <- unique(unlist(ids2))
  return(ids2)
}


pts.b.ab1.id2 <- hospids(pts.b.ab1)
pts.b.ab2.id2 <- hospids(pts.b.ab2)
pts.b.ab0.id2 <- hospids(pts.b.ab0)

length(ptid.b.ab1.hosp_mortality) / length(pts.b.ab1.id2)
length(ptid.b.ab2.hosp_mortality) / length(pts.b.ab2.id2)
length(ptid.b.ab0.hosp_mortality) / length(pts.b.ab0.id2)


ptid.b.ab1.30_mortality <- intersect(pts.b.ab1, df1.mortality30.id) 
ptid.b.ab2.30_mortality <- intersect(pts.b.ab2, df1.mortality30.id) 
ptid.b.ab0.30_mortality <- intersect(pts.b.ab0, df1.mortality30.id) 



length(ptid.b.ab1.30_mortality) / length(pts.b.ab1)
length(ptid.b.ab2.30_mortality) / length(pts.b.ab2)
length(ptid.b.ab0.30_mortality) / length(pts.b.ab0)




pts.b.ab0.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab0, c(19, 10:15)]
pts.b.ab0.scores$ab <- 0
pts.b.ab1.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab1, c(19, 10:15)]
pts.b.ab1.scores$ab <- 1
pts.b.ab2.scores <- ptinfos2[ptinfos2$id %in% pts.b.ab2, c(19, 10:15)]
pts.b.ab2.scores$ab <- 2

pts.b.ab.scores <- rbind(pts.b.ab0.scores, pts.b.ab1.scores)
pts.b.ab.scores <- rbind(pts.b.ab.scores, pts.b.ab2.scores)


pts.b.ab.scores$ab2 <- as.factor(pts.b.ab.scores$ab)
str(pts.b.ab.scores)
library(ggplot2)
cdat <- ddply(pts.b.ab.scores, "ab2", summarise, rating.mean=mean(sapsi_max, na.rm = T)) 
p1 <- ggplot(pts.b.ab.scores, aes(x=sapsi_max, colour=ab2)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=ab2),
             linetype="dashed", size=1)

cdat <- ddply(pts.b.ab.scores, "ab2", summarise, rating.mean=mean(sapsi_first, na.rm = T)) 
p2 <- ggplot(pts.b.ab.scores, aes(x=sapsi_first, colour=ab2)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=ab2),
             linetype="dashed", size=1)

cdat <- ddply(pts.b.ab.scores, "ab2", summarise, rating.mean=mean(sofa_max, na.rm = T)) 
p3 <- ggplot(pts.b.ab.scores, aes(x=sofa_max, colour=ab2)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=ab2),
             linetype="dashed", size=1)

cdat <- ddply(pts.b.ab.scores, "ab2", summarise, rating.mean=mean(sofa_first, na.rm = T)) 
p4 <- ggplot(pts.b.ab.scores, aes(x=sofa_first, colour=ab2)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=ab2),
             linetype="dashed", size=1)

multiplot(p1, p2, p3, p4, cols=2)


# 
# 
# library('RColorBrewer')
# 
# 
# ptids$freq2 <- 0
# antibiotics5a.count2 <- merge(antibiotics5a.count, ptids, all.x = T, all.Y = T, by.x = 'id', by.y = 'id')
# 
# myPalette <- colorRampPalette(rev(brewer.pal(10, "Spectral")), space="Lab")
# unique(antibiotics5a.count2$id)
# 
# zp1 <- ggplot(antibiotics5a.count,
#               aes(x = id, y = itemidG, fill = freq), title('Numbers of Antibiotics Over Entire Visit Before Clearance')) + theme(axis.text.y=element_text(size=500))
# zp1 <- zp1 + geom_tile()
# zp1 <- zp1 + scale_fill_gradientn(colours = myPalette(100))
# # zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
# # zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
# # zp1 <- zp1 + coord_equal()
# zp1 <- zp1 + theme_bw()
# print(zp1)
# 
# 
# 
# antibiotics5a <- antibiotics5[is.na(antibiotics5$dftime) | (!is.na(antibiotics5$dftime) & antibiotics5$dftime < 0),]
# unique(antibiotics4a$id)
# length(unique(antibiotics4a$id[!is.na(antibiotics4a$dftime)]))
# str(antibiotics4a)
# ptids <- data.frame(c(as.character(unique(antibiotics4$id))))
# names(ptids) <- 'id'
# str(ptids)
# antibiotics4b <- merge(antibiotics4a, ptids, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
# str(antibiotics4b)
# antibiotics4b.count <- count(antibiotics4b, c('id', 'itemidG'))
# str(antibiotics4b.count)
# 
# zp2 <- ggplot(antibiotics4b.count,
#               aes(x = id, y = itemidG, fill = freq), title('Numbers of Antibiotics Prior to Event')) + theme(axis.text.y=element_text(size=500))
# zp2 <- zp2 + geom_tile()
# zp2 <- zp2 + scale_fill_gradientn(colours = myPalette(100))
# # zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
# # zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
# # zp1 <- zp1 + coord_equal()
# zp2 <- zp2 + theme_bw()
# print(zp2)
# 
# 
# 
# antibiotics4c <- antibiotics4[is.na(antibiotics4$dftime) | (!is.na(antibiotics4$dftime) & antibiotics4$dftime >= 0),]
# length(unique(antibiotics4c$id))
# length(unique(antibiotics4c$id[!is.na(antibiotics4c$dftime)]))
# str(antibiotics4c)
# ptids <- data.frame(c(as.character(unique(antibiotics4$id))))
# names(ptids) <- 'id'
# str(ptids)
# antibiotics4d <- merge(antibiotics4c, ptids, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
# str(antibiotics4d)
# antibiotics4d.count <- count(antibiotics4d, c('id', 'itemidG'))
# str(antibiotics4d.count)
# 
# zp3 <- ggplot(antibiotics4d.count,
#               aes(x = id, y = itemidG, fill = freq), title('Numbers of Antibiotics After Event')) + theme(axis.text.y = element_text(size=500))
# zp3 <- zp3 + geom_tile()
# zp3 <- zp3 + scale_fill_gradientn(colours = myPalette(100))
# # zp1 <- zp1 + scale_x_discrete(expand = c(0, 0))
# # zp1 <- zp1 + scale_y_discrete(expand = c(0, 0))
# # zp1 <- zp1 + coord_equal()
# zp3 <- zp3 + theme_bw()
# print(zp3)
#
# firstantibiotic.bypt.bytype <- function(dataset) {
#   first<- dataset[1,]   # x = time of event
#   return(first)
# }
# 
# # antibiotics6 <- ddply(antibiotics5, .(id, itemid), firstantibiotic.bypt.bytype)
# antibiotics6B <- ddply(antibiotics5, .(id), firstantibiotic.bypt.bytype)
# head(antibiotics6B)
# #to model the relationship between time to antibiotic and time to 10% lactate clearance
# #hence, remove the obs. in which clearance is earlier than antibiotic is given
# 
# 
# #==============================model the relationship between antibiotics and 10% lactate clearance time (transformed to multi-class) =============================
# antibiotics7 <- antibiotics6[(antibiotics6$dftime < antibiotics6$cleartime) | is.na(antibiotics6$cleartime), ]
# antibiotics6B$response <- NULL
# for (i in 1:nrow(antibiotics6B)) {
#   if ((!is.na(antibiotics6B$cleartime[i])) & (antibiotics6B$cleartime[i] <= 6)) {
#     antibiotics6B$respone[i] <- '1'
#   } else if ((!is.na(antibiotics6B$cleartime[i])) & (antibiotics6B$cleartime[i] <= 12)) {
#     antibiotics6B$respone[i] <- '2'
#   } else if ((!is.na(antibiotics6B$cleartime[i])) & (antibiotics6B$cleartime[i] <= 24)) {
#     antibiotics6B$respone[i] <- '3'
#   } else if (((!is.na(antibiotics6B$cleartime[i])) & (antibiotics6B$cleartime[i] > 24)) | ((is.na(antibiotics6B$cleartime[i])) & (antibiotics6B$lastlactate.dftime[i] >= 24))) {
#     antibiotics6B$respone[i] <- '4'
#   } else if ((is.na(antibiotics6B$cleartime[i])) & (!is.na(antibiotics6B$lastlactate.dftime[i])) & (antibiotics6B$lastlactate.dftime[i] < 24)) {
#     antibiotics6B$respone[i] <- '5'
#   }
#     else {
#     antibiotics6B$respone[i] <- '6'
#   }
# }
# antibiotics6B <- antibiotics6B[order(antibiotics6B$respone),]
# 
# 
# abdata <- antibiotics6B[, c(1, 4, 6, 7, 10, 11)]
# str(abdata)
# abdata$dftime <- as.numeric(abdata$dftime)
# abdata$respone <- as.character(abdata$respone)
# table(abdata$respone)
# 
# abdata2 <- abdata[c(1:1140),] # remove those without a valid lactate clear time due to insufficient lactate tests
# abdata2$dftime[is.na(abdata2$itemid)] <- 1000
# abdata2$itemid2 <- as.character(abdata2$itemid)
# abdata2$itemid2[is.na(abdata2$itemid)] <- 'AAAAA'
# abdata2$itemid2 <-as.factor(abdata2$itemid2)
# abdata2 <- abdata2[order(abdata2$respone),]
# head(abdata2)
# #predictive models 
# library(nnet)
# abdata2$respone <- as.factor(abdata2$respone)
# abdata2$respone2 <- relevel(abdata2$respone, ref = '4')
# test = multinom(respone2 ~ dftime + initial.lactate + itemid2, data = abdata2)
# summary(test)
# z <- summary(test)$coefficients/summary(test)$standard.errors
# p <- (1 - pnorm(abs(z), 0, 1)) * 2
# p
# 
# #==========================model the relationship between antibiotics and lactate normalization time (transformed to multi-class) =============================
# antibiotics6C$responseB <- NULL
# for (i in 1:nrow(antibiotics6B)) {
#   if ((!is.na(antibiotics6B$normalizetime[i])) & (antibiotics6B$normalizetime[i] <= 6)) {
#     antibiotics6B$responeB[i] <- '1'
#   } else if ((!is.na(antibiotics6B$normalizetime[i])) & (antibiotics6B$normalizetime[i] <= 12)) {
#     antibiotics6B$responeB[i] <- '2'
#   } else if ((!is.na(antibiotics6B$normalizetime[i])) & (antibiotics6B$normalizetime[i] <= 24)) {
#     antibiotics6B$responeB[i] <- '3'
#   } else if (((!is.na(antibiotics6B$normalizetime[i])) & (antibiotics6B$normalizetime[i] > 24)) | ((is.na(antibiotics6B$normalizetime[i])) & (antibiotics6B$lastlactate.dftime[i] >= 24))) {
#     antibiotics6B$responeB[i] <- '4'
#   } else if ((is.na(antibiotics6B$normalizetime[i])) & (!is.na(antibiotics6B$lastlactate.dftime[i])) & (antibiotics6B$lastlactate.dftime[i] < 24)) {
#     antibiotics6B$responeB[i] <- '5'
#   }
#   else {
#     antibiotics6B$responeB[i] <- '6'
#   }
# }
# antibiotics6B <- antibiotics6B[order(antibiotics6B$responeB),]
# head(antibiotics6B)
# 
# abdataB <- antibiotics6B[, c(1, 4, 6, 7, 10, 12)]
# str(abdataB)
# abdataB$dftime <- as.numeric(abdataB$dftime)
# abdataB$responeB <- as.character(abdataB$responeB)
# table(abdataB$responeB)
# 
# abdataB2 <- abdataB[c(1:1026),] # remove those without a valid lactate clear time due to insufficient lactate tests
# abdataB2$dftime[is.na(abdataB2$itemid)] <- 1000
# abdataB2$itemid2 <- as.character(abdataB2$itemid)
# abdataB2$itemid2[is.na(abdataB2$itemid)] <- 'AAAAA'
# abdataB2$itemid2 <-as.factor(abdataB2$itemid2)
# abdataB2 <- abdataB2[order(abdataB2$responeB),]
# head(abdataB2)
# #predictive models 
# library(nnet)
# abdataB2$responeB <- as.factor(abdataB2$responeB)
# abdataB2$responeB2 <- relevel(abdataB2$responeB, ref = '4')
# testB = multinom(responeB2 ~ dftime + initial.lactate + itemid2, data = abdataB2)
# summary(testB)
# zB <- summary(testB)$coefficients/summary(testB)$standard.errors
# pB <- (1 - pnorm(abs(zB), 0, 1)) * 2
# pB
# 
# 
# #=============================
# 
# 
# 
# 
# #imputation to be done=====================================
# # require(DMwR)
# # data.l1 <- knnImputation(data.l, k=63, scale=T, meth="weighAvg")
# 
# #===================intraveneous fliuds=======================================================
# fluids <- read.csv("sepsisptsdf1_fliuds.csv")
# fluids$id <- paste(fluids$subject_id, fluids$hospital_seq, fluids$icustay_seq, sep='#%#')
# fluids2 <- data.frame(fluids$id, fluids$charttime, fluids$itemid, fluids$volume, fluids$volumeuom)
# names(fluids2) <- c('id', 'charttime', 'itemid', 'volume', 'volumeuom')
# summary(fluids2$volumeuom)
# #remove all with missing or 0 volume
# fluids3 <- fluids2[!is.na(fluids2$volume) & (fluids2$volume > 0), ]
# #remove column volumeuom since all fluids in fluids3 are in ml
# fluids3$volumeuom <- NULL
# 
# fluids4 <- merge(fluids3, sepsis.time, by.x = 'id', by.y = 'id', all.x = T)
# fluids4$charttime <- ymd_hms(fluids4$charttime)
# fluids4$dftime <- difftime(fluids4$charttime, fluids4$event.time, units = 'hours')
# fluids5 <- fluids4[fluids4$dftime >= 0, ]
# str(fluids5)
# summary(fluids5$itemid)
# fluids5 <- merge(fluids5, lactateclearance, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
# 
# firstfliud.bypt.bytype <- function(dataset) {
#   first<- dataset[1,]   # x = time of event
#   return(first)
# }
# 
# fluids6 <- ddply(fluids5, .(id, itemid), firstfluid.bypt.bytype)
# fluids6B <- ddply(fluids5, .(id), firstfliud.bypt.bytype)
# head(fliuds6B)
# #to model the relationship between time to antibiotic and time to 10% lactate clearance
# #hence, remove the obs. in which clearance is earlier than fliud is given
# 
# 
# #==============================model the relationship between antibiotics and 10% lactate clearance time (transformed to multi-class) =============================ant
# fluids6B$response <- NULL
# for (i in 1:nrow(fluids6B)) {
#   if ((!is.na(fluids6B$cleartime[i])) & (fluids6B$cleartime[i] <= 6)) {
#     fluids6B$respone[i] <- '1'
#   } else if ((!is.na(fluids6B$cleartime[i])) & (fluids6B$cleartime[i] <= 12)) {
#     fluids6B$respone[i] <- '2'
#   } else if ((!is.na(fluids6B$cleartime[i])) & (fluids6B$cleartime[i] <= 24)) {
#     fluids6B$respone[i] <- '3'
#   } else if (((!is.na(fluids6B$cleartime[i])) & (fluids6B$cleartime[i] > 24)) | ((is.na(fluids6B$cleartime[i])) & (fluids6B$lastlactate.dftime[i] >= 24))) {
#     fluids6B$respone[i] <- '4'
#   } else if ((is.na(fluids6B$cleartime[i])) & (!is.na(fluids6B$lastlactate.dftime[i])) & (fluids6B$lastlactate.dftime[i] < 24)) {
#     fluids6B$respone[i] <- '5'
#   }
#   else {
#     fluids6B$respone[i] <- '6'
#   }
# }
# fluids6B <- fluids6B[order(fluids6B$respone),]
# 
# head(fluids6B)
# fluiddata <- fluids6B[, c(1, 3, 4, 6, 7, 10, 11)]
# str(fluiddata)
# fluiddata$dftime <- as.numeric(fluiddata$dftime)
# fluiddata$respone <- as.character(fluiddata$respone)
# fluiddata$itemid <- as.factor(fluiddata$itemid)
# table(fluiddata$respone)
# 
# fluiddata2 <- fluiddata[c(1:1140),] # remove those without a valid lactate clear time due to insufficient lactate tests
# fluiddata2$itemid2 <- fluiddata2$itemid
# 
# fluiddata2 <- fluiddata2[order(fluiddata2$id),]
# head(fluiddata2)
# abdata2 <- abdata2[order(abdata2$id),]
# head(abdata2)
# abfluids <- merge(abdata2, fluiddata2, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
# abfluids2 <- data.frame(abfluids$id, abfluids$initial.lactate.x, abfluids$itemid2.x, abfluids$dftime.x, abfluids$itemid.y, abfluids$volume, abfluids$dftime.y, abfluids$respone.x)
# names(abfluids2) <- c('id', 'initial.lactate', 'antibiotic', 'abdftime', 'fluid', 'volume', 'fluiddftime', 'respone')
# abfluids2$fluiddftime[is.na(abfluids2$fluiddftime)] <- 1000
# abfluids2$fluid2 <- as.character(abfluids2$fluid)
# abfluids2$fluid2[is.na(abfluids2$fluid)] <- '00'
# abfluids2$fluid2 <-as.factor(abfluids2$fluid2)
# 
# 
# 
# #predictive models 
# library(nnet)
# abfluids2$respone <- as.factor(abfluids2$respone)
# abfluids2$respone2 <- relevel(abfluids2$respone, ref = '4')
# head(abfluids2)
# test = multinom(respone2 ~ antibiotic + abdftime + fluid2 + fluiddftime + initial.lactate, data = abfluids2)
# summary(test)
# z <- summary(test)$coefficients/summary(test)$standard.errors
# p <- (1 - pnorm(abs(z), 0, 1)) * 2
# p
# 
# #==========================model the relationship between fluids and lactate normalization time (transformed to multi-class) =============================
# fluids6B$responseB <- NULL
# for (i in 1:nrow(fluids6B)) {
#   if ((!is.na(fluids6B$normalizetime[i])) & (fluids6B$normalizetime[i] <= 6)) {
#     fluids6B$responeB[i] <- '1'
#   } else if ((!is.na(fluids6B$normalizetime[i])) & (fluids6B$normalizetime[i] <= 12)) {
#     fluids6B$responeB[i] <- '2'
#   } else if ((!is.na(fluids6B$normalizetime[i])) & (fluids6B$normalizetime[i] <= 24)) {
#     fluids6B$responeB[i] <- '3'
#   } else if (((!is.na(fluids6B$normalizetime[i])) & (fluids6B$normalizetime[i] > 24)) | ((is.na(fluids6B$normalizetime[i])) & (fluids6B$lastlactate.dftime[i] >= 24))) {
#     fluids6B$responeB[i] <- '4'
#   } else if ((is.na(fluids6B$normalizetime[i])) & (!is.na(fluids6B$lastlactate.dftime[i])) & (fluids6B$lastlactate.dftime[i] < 24)) {
#     fluids6B$responeB[i] <- '5'
#   }
#   else {
#     fluids6B$responeB[i] <- '6'
#   }
# }
# fluids6B <- fluids6B[order(fluids6B$responeB),]
# 
# head(fluids6B)
# fluiddataB <- fluids6B[, c(1, 3, 4, 6, 7, 10, 12)]
# str(fluiddataB)
# fluiddataB$dftime <- as.numeric(fluiddataB$dftime)
# fluiddataB$responeB <- as.character(fluiddataB$responeB)
# fluiddataB$itemid <- as.factor(fluiddataB$itemid)
# table(fluiddataB$responeB)
# 
# fluiddataB2 <- fluiddataB[c(1:1026),] # remove those without a valid lactate clear time due to insufficient lactate tests
# fluiddataB2$itemid2 <- fluiddataB2$itemid
# 
# fluiddataB2 <- fluiddataB2[order(fluiddataB2$id),]
# head(fluiddataB2)
# abdataB2 <- abdataB2[order(abdataB2$id),]
# head(abdataB2)
# abfluidsB <- merge(abdataB2, fluiddataB2, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
# names(abfluidsB)
# abfluidsB2 <- data.frame(abfluidsB$id, abfluidsB$initial.lactate.x, abfluidsB$itemid2.x, abfluidsB$dftime.x, abfluidsB$itemid.y, abfluidsB$volume, abfluidsB$dftime.y, abfluidsB$responeB.x)
# names(abfluidsB2) <- c('id', 'initial.lactate', 'antibiotic', 'abdftime', 'fluid', 'volume', 'fluiddftime', 'respone')
# abfluidsB2$fluiddftime[is.na(abfluidsB2$fluiddftime)] <- 1000
# abfluidsB2$fluid2 <- as.character(abfluidsB2$fluid)
# abfluidsB2$fluid2[is.na(abfluidsB2$fluid)] <- '00'
# abfluidsB2$fluid2 <-as.factor(abfluidsB2$fluid2)
# 
# 
# 
# #predictive models 
# library(nnet)
# abfluidsB2$respone <- as.factor(abfluidsB2$respone)
# abfluidsB2$respone2 <- relevel(abfluidsB2$respone, ref = '4')
# head(abfluidsB2)
# abfluidsB2$id <- as.character(abfluidsB2$id)
# 
# testB = multinom(respone2 ~ antibiotic + abdftime + fluid2 + volume + fluiddftime + initial.lactate, data = abfluidsB2)
# summary(testB)
# zB <- summary(testB)$coefficients/summary(testB)$standard.errors
# pB <- (1 - pnorm(abs(zB), 0, 1)) * 2
# pB
# res = predict(testB, abfluidsB2[1,], 'probs')
# res2 = c(res, abfluidsB2[1,1])
# res2
# res
# head(res)
# summary(fluids6B$cleartime)
# 
# #cross-validation for lactate 10% decrease prediction
# lactateclearance.predict <- function(data) {
#   predictions <- data.frame(matrix(NA, ncol=4))
#   # names(predictions) <- c(1,2,3,4)
#   
#   ids <- c(NA)
#   labels <- c(NA)
#   data <- data[sample.int(nrow(data)),]
#   for (i in 1:nrow(data)) {
#     train <- data[-i, ]
#     test <- data[i, ]
#     model <- multinom(respone2 ~ antibiotic + abdftime + fluid2 + volume + fluiddftime + initial.lactate, train)
#     res <- predict(model, test, 'probs')
#     predictions <- rbind(predictions, c(res))
#     ids <- c(ids, test[1])
#     labels <- c(labels, test[10])
#   }
#   predictions$id <- ids
#   predictions$label <- labels
#   predictions <- predictions[-1,]
#   return(predictions)
# }
# summary(abfluidsB2$respone2)
# results.clearance0 <- lactateclearance.predict(abfluidsB2[1:8,])
# results.normalize <- lactateclearance.predict(abfluids2)
# results.clearance$label0 <- rev(abfluidsB2$respone2)
# results.clearance2 <- results.clearance[,-6]
# summary(data0$label)
# #calculate the prediction performance 
# classifier.performane <- function(data) {
#   names(data) <- c('Cls4', 'Cls1', 'Cls2', 'Cls3', 'id', 'label')
#   data$label <- as.character(data$label)
#   data$pred <- apply(data[,(1:4)],1,max)
#   t1p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '1'), ])
#   t1p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '1'), ])
#   t1p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '1'), ])
#   t1p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '1'), ])
#   t2p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '2'), ])
#   t2p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '2'), ])
#   t2p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '2'), ])
#   t2p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '2'), ])
#   t3p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '3'), ])
#   t3p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '3'), ])
#   t3p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '3'), ])
#   t3p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '3'), ])
#   t4p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '4'), ])
#   t4p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '4'), ])
#   t4p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '4'), ])
#   t4p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '4'), ])
#   performance <- c(t1p1, t1p2, t1p3, t1p4, t2p1, t2p2, t2p3, t2p4, t3p1, t3p2, t3p3, t3p4, t4p1, t4p2, t4p3, t4p4)
#   return(performance)
# }
# 
# performance.clearance <- classifier.performane(results.clearance2)
# results.clearance$label <- as.character(results.clearance)
# summary(results.clearance$label)
# head(results.clearance)
# 
# #========================================================================================================================================================
# for (i in 1:nrow(fluids6B)) {
#     if ((!is.na(fluids6B$normalizetime[i])) & (fluids6B$normalizetime[i] <= 12)) {
#     fluids6B$responeC[i] <- '1'
#   } else if (((!is.na(fluids6B$normalizetime[i])) & (fluids6B$normalizetime[i] > 12)) | ((is.na(fluids6B$normalizetime[i])) & (fluids6B$lastlactate.dftime[i] >= 12))) {
#     fluids6B$responeC[i] <- '2'
#   } else if ((is.na(fluids6B$normalizetime[i])) & (!is.na(fluids6B$lastlactate.dftime[i])) & (fluids6B$lastlactate.dftime[i] < 12)) {
#     fluids6B$responeC[i] <- '3'
#   }
#   else {
#     fluids6B$responeC[i] <- '4'
#   }
# }
# fluids6B <- fluids6B[order(fluids6B$responeC),]
# 
# head(fluids6B)
# fluiddataB <- fluids6B[, c(1, 3, 4, 6, 7, 10, 11)]
# str(fluiddataB)
# fluiddataB$dftime <- as.numeric(fluiddataB$dftime)
# fluiddataB$responeC <- as.character(fluiddataB$responeC)
# fluiddataB$itemid <- as.factor(fluiddataB$itemid)
# table(fluiddataB$responeC)
# 
# fluiddataB3 <- fluiddataB[c(1:1118),] # remove those without a valid lactate clear time due to insufficient lactate tests
# fluiddataB3$itemid2 <- fluiddataB3$itemid
# 
# fluiddataB3 <- fluiddataB3[order(fluiddataB3$id),]
# head(fluiddataB3)
# 
# 
# 
# abdataB3 <- abdataB3[order(abdataB3$id),]
# head(abdataB3)
# 
# abfluidsC <- merge(abdataB3, fluiddataB3, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
# names(abfluidsC)
# abfluidsC2 <- data.frame(abfluidsC$id, abfluidsC$initial.lactate.x, abfluidsC$itemid2.x, abfluidsC$dftime.x, abfluidsC$itemid.y, abfluidsC$volume, abfluidsC$dftime.y, abfluidsC$responeC.x)
# names(abfluidsC2) <- c('id', 'initial.lactate', 'antibiotic', 'abdftime', 'fluid', 'volume', 'fluiddftime', 'respone')
# abfluidsC2$fluiddftime[is.na(abfluidsC2$fluiddftime)] <- 1000
# abfluidsC2$fluid2 <- as.character(abfluidsC2$fluid)
# abfluidsC2$fluid2[is.na(abfluidsC2$fluid)] <- '00'
# abfluidsC2$fluid2 <-as.factor(abfluidsC2$fluid2)
# 
# 
# 
# #predictive models 
# library(nnet)
# abfluidsB2$respone <- as.factor(abfluidsB2$respone)
# abfluidsB2$respone2 <- relevel(abfluidsB2$respone, ref = '4')
# head(abfluidsB2)
# abfluidsB2$id <- as.character(abfluidsB2$id)
# testB2 <- glm(respone2 ~ antibiotic + abdftime + fluid2 + volume + fluiddftime + initial.lactate, data = abfluidsB2, family='binomial')
# 
# testB = multinom(respone2 ~ antibiotic + abdftime + fluid2 + volume + fluiddftime + initial.lactate, data = abfluidsB2)
# summary(testB2)
# zB2 <- summary(testB2)$coefficients/summary(testB2)$standard.errors
# pB2 <- (1 - pnorm(abs(zB2), 0, 1)) * 2
# pB2
# res = predict(testB, abfluidsB2[1,], 'probs')
# res2 = c(res, abfluidsB2[1,1])
# res2
# res
# head(res)
# summary(fluids6B$cleartime)
# 
# #cross-validation for lactate 10% decrease prediction
# lactateclearance.predict <- function(data) {
#   predictions <- data.frame(matrix(NA, ncol=4))
#   # names(predictions) <- c(1,2,3,4)
#   
#   ids <- c(NA)
#   labels <- c(NA)
#   data <- data[sample.int(nrow(data)),]
#   for (i in 1:nrow(data)) {
#     train <- data[-i, ]
#     test <- data[i, ]
#     model <- multinom(respone2 ~ antibiotic + abdftime + fluid2 + volume + fluiddftime + initial.lactate, train)
#     res <- predict(model, test, 'probs')
#     predictions <- rbind(predictions, c(res))
#     ids <- c(ids, test[1])
#     labels <- c(labels, test[10])
#   }
#   predictions$id <- ids
#   predictions$label <- labels
#   predictions <- predictions[-1,]
#   return(predictions)
# }
# summary(abfluidsB2$respone2)
# results.clearance0 <- lactateclearance.predict(abfluidsB2[1:8,])
# results.normalize <- lactateclearance.predict(abfluids2)
# results.clearance$label0 <- rev(abfluidsB2$respone2)
# results.clearance2 <- results.clearance[,-6]
# summary(data0$label)
# #calculate the prediction performance 
# classifier.performane <- function(data) {
#   names(data) <- c('Cls4', 'Cls1', 'Cls2', 'Cls3', 'id', 'label')
#   data$label <- as.character(data$label)
#   data$pred <- apply(data[,(1:4)],1,max)
#   t1p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '1'), ])
#   t1p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '1'), ])
#   t1p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '1'), ])
#   t1p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '1'), ])
#   t2p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '2'), ])
#   t2p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '2'), ])
#   t2p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '2'), ])
#   t2p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '2'), ])
#   t3p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '3'), ])
#   t3p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '3'), ])
#   t3p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '3'), ])
#   t3p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '3'), ])
#   t4p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '4'), ])
#   t4p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '4'), ])
#   t4p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '4'), ])
#   t4p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '4'), ])
#   performance <- c(t1p1, t1p2, t1p3, t1p4, t2p1, t2p2, t2p3, t2p4, t3p1, t3p2, t3p3, t3p4, t4p1, t4p2, t4p3, t4p4)
#   return(performance)
# }
# 
# performance.clearance <- classifier.performane(results.clearance2)
# results.clearance$label <- as.character(results.clearance)
# summary(results.clearance$label)
# head(results.clearance)
# 
# #==============================model the relationship between antibiotics and 10% lactate clearance time (transformed to multi-class) =============================ant
# abafluid <- merge(antibiotics6B, fluids6B, by.x = 'id', by.y = 'id', all.x = T, all.y = T)
# names(abafluid)
# 
# abafluid2 <- data.frame(abafliud$id, abafluid$itemid.x, abafliud$dftime.x, abafliud$itemid.y, abafluid$volume, abafliud$dftime.y, abafliud$initial.lactate.x, abafliud$cleartime.x, abafliud$normalizetime.x, abafliud$lastlactate.dftime.x)
# names(abafluid2) <- c('id', 'antibiotic', 'abdftime', 'fluid', 'volume', 'fluiddftime', 'initial.lactate', 'cleartime', 'normalizetime', 'lastlactate.dftime')
# 
# abafluid2$abdftime[is.na(abafluid2$antibiotic)] <- 1000
# abafluid2$antibiotic2 <- as.character(abafluid2$antibiotic)
# abafluid2$antibiotic2[is.na(abafluid2$antibiotic)] <- 'AAAAA'
# abafluid2$antibiotic2 <-as.factor(abafluid2$antibiotic2)
# head(abafluid2)
# 
# abafluid2$fluiddftime[is.na(abafluid2$fluid)] <- 1000
# abafluid2$volume[is.na(abafluid2$fluid)] <- 0
# abafluid2$fluid2 <- as.character(abafluid2$fluid)
# abafluid2$fluid2[is.na(abafluid2$fluid)] <- '00'
# abafluid2$fluid2 <-as.factor(abafluid2$fluid2)
# 
# 
# for (i in 1:nrow(abafluid2)) {
#   if ((!is.na(abafluid2$normalizetime[i])) & (abafluid2$normalizetime[i] <= 12)) {
#     abafluid2$responeC[i] <- '1'
#   } else if (((!is.na(abafluid2$normalizetime[i])) & (abafluid2$normalizetime[i] > 12)) | ((is.na(abafluid2$normalizetime[i])) & (abafluid2$lastlactate.dftime[i] >= 12))) {
#     abafluid2$responeC[i] <- '2'
#   } else if ((is.na(abafluid2$normalizetime[i])) & (!is.na(abafluid2$lastlactate.dftime[i])) & (abafluid2$lastlactate.dftime[i] < 12)) {
#     abafluid2$responeC[i] <- '3'
#   }
#   else {
#     abafluid2$responeC[i] <- '4'
#   }
# }
# abafluid2 <- abafluid2[order(abafluid2$responeC),]
# 
# head(abafluid2)
# unique(abafluid2$antibiotic2)
# 
# str(abafluid2)
# abafluid2$abdftime <- as.numeric(abafluid2$abdftime)
# abafluid2$fluiddftime <- as.numeric(abafluid2$fluiddftime)
# abafluid2$responeC <- as.character(abafluid2$responeC)
# abafluid2$antibiotic2 <- as.factor(abafluid2$antibiotic2)
# abafluid2$fluid2 <- as.factor(abafluid2$fluid2)
# table(abafluid2$responeC)
# 
# abafluid3 <- abafluid2[c(1:1118),] # remove those without a valid lactate clear time due to insufficient lactate tests
# 
# head(abafluid3)
# 
# 
# #predictive models 
# library(nnet)
# abafluid3$responeC <- as.factor(abafluid3$responeC)
# abafluid3$responeC2 <- relevel(abafluid3$responeC, ref = '2')
# str(abafluid3)
# abafluid3$id <- as.character(abafluid3$id)
# 
# testC <- glm(responeC2 ~ antibiotic2 + abdftime + fluid2 + volume + fluiddftime + initial.lactate, data = abafluid3, family='binomial')
# testC2 <- glm(responeC2 ~ antibiotic2 + abdftime + initial.lactate, data = abafluid3, family='binomial')
# 
# summary(testC2)
# unique(abafluid3$fluid2)
# library(aod)
# wald.test(b = coef(testC), Sigma = vcov(testC), Terms = 42:114)
# dim(confint(testC))
# #cross-validation for lactate 10% decrease prediction
# lactateclearance.predict <- function(data) {
#   predictions <- data.frame(matrix(NA, ncol=2))
#   # names(predictions) <- c(1,2,3,4)
#   
#   ids <- c(NA)
#   labels <- c(NA)
#   data <- data[sample.int(nrow(data)),]
#   for (i in 1:nrow(data)) {
#     train <- data[-i, ]
#     test <- data[i, ]
#     model <- multinom(respone2 ~ antibiotic + abdftime + fluid2 + volume + fluiddftime + initial.lactate, train)
#     res <- predict(model, test, 'probs')
#     predictions <- rbind(predictions, c(res))
#     ids <- c(ids, test[1])
#     labels <- c(labels, test[10])
#   }
#   predictions$id <- ids
#   predictions$label <- labels
#   predictions <- predictions[-1,]
#   return(predictions)
# }
# summary(abfluidsB2$respone2)
# results.clearance0 <- lactateclearance.predict(abfluidsB2[1:8,])
# results.normalize <- lactateclearance.predict(abfluids2)
# results.clearance$label0 <- rev(abfluidsB2$respone2)
# results.clearance2 <- results.clearance[,-6]
# summary(data0$label)
# #calculate the prediction performance 
# classifier.performane <- function(data) {
#   names(data) <- c('Cls4', 'Cls1', 'Cls2', 'Cls3', 'id', 'label')
#   data$label <- as.character(data$label)
#   data$pred <- apply(data[,(1:4)],1,max)
#   t1p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '1'), ])
#   t1p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '1'), ])
#   t1p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '1'), ])
#   t1p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '1'), ])
#   t2p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '2'), ])
#   t2p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '2'), ])
#   t2p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '2'), ])
#   t2p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '2'), ])
#   t3p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '3'), ])
#   t3p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '3'), ])
#   t3p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '3'), ])
#   t3p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '3'), ])
#   t4p1 = nrow(data[(data$pred == data$Cls1) & (data$label == '4'), ])
#   t4p2 = nrow(data[(data$pred == data$Cls2) & (data$label == '4'), ])
#   t4p3 = nrow(data[(data$pred == data$Cls3) & (data$label == '4'), ])
#   t4p4 = nrow(data[(data$pred == data$Cls4) & (data$label == '4'), ])
#   performance <- c(t1p1, t1p2, t1p3, t1p4, t2p1, t2p2, t2p3, t2p4, t3p1, t3p2, t3p3, t3p4, t4p1, t4p2, t4p3, t4p4)
#   return(performance)
# }
# 
# performance.clearance <- classifier.performane(results.clearance2)
# results.clearance$label <- as.character(results.clearance)
# summary(results.clearance$label)
# head(results.clearance)
# 
# 
# abafluid3 <- abafluid2[!is.na(abafluid2$cleartime),]
# fit <- lm(cleartime ~ antibiotic2 + abdftime + fluid2 + volume + fluiddftime + initial.lactate, data = abafluid3)
# summary(fit)
# anova(fit)
# plot(fit)
# 
# 
# abafluid4 <- abafluid2[!is.na(abafluid2$normalizetime),]
# fit2 <- lm(cleartime ~ antibiotic2 + abdftime + initial.lactate, data = abafluid4)
# summary(fit2)
# anova(fit2)
# plot(fit2)
# 
# 
# #==================create four patient cohorts: 10% lactate decrease within 0~6 hrs; 6~12 hrs; 0~24 hrs; not cleared within 0~24 hrs;
# #================first imputation for those without enough observations using KNN=============
# 

# #===================== Cox proportional method to model the probability of 12-hour 10% lactate clearance=================================
# clear.pts <- fluids7[(!is.na(antibiotics7$cleartime) & antibiotics7$cleartime <= 12),]
# noclear.pts <- antibiotics7[(!is.na(antibiotics7$cleartime) & antibiotics7$cleartime > 12) | (is.na(antibiotics7$cleartime) & antibiotics7$lastlactate.dftime > 12),]
# right.censored.pts <- antibiotics7[(is.na(antibiotics7$cleartime) & antibiotics7$lastlactate.dftime <= 12),] # right censoring which can be either end-of-study censoring or loss-to-follow-up censoring
# 
# install.packages('MIICD')
# library(MIICD)
# data(ICCRD)
# d <- ICCRD[ICCRD$treatment=='tr1',]
# res <- DA.ci(k = 10, m = 10, status = 'status', trans = 1 , data = ICCRD,
#              conf.int = TRUE, cens.code = 0 , alpha = 0.05)
# res # print(res)
# plot(res)
# 
# 
# #===================== Cox proportional method to model the probability of 12-hour 10% lactate clearance=================================
# clear.pts <- antibiotics7[(!is.na(antibiotics7$cleartime) & antibiotics7$cleartime <= 12),]
# noclear.pts <- antibiotics7[(!is.na(antibiotics7$cleartime) & antibiotics7$cleartime > 12) | (is.na(antibiotics7$cleartime) & antibiotics7$lastlactate.dftime > 12),]
# censored.pts <- antibiotics7[(is.na(antibiotics7$cleartime) & antibiotics7$lastlactate.dftime <= 12),]
# 
# cleardata <- data.frame(t(c(clear.pts$itemid[1], clear.pts$dftime[1], 2, clear.pts$cleartime[1])))
# names(cleardata) <- c('itemid', 'dftime', 'status', 'cleartime')
# for (i in 2:nrow(clear.pts)) {
#   cleardata0 <- c(clear.pts$itemid[i], clear.pts$dftime[i], 2, clear.pts$cleartime[i])
#   cleardata <- rbind(cleardata, cleardata0)
# }
# 
# 
# nocleardata <- data.frame(t(c(noclear.pts$itemid[1], noclear.pts$dftime[1], 1, noclear.pts$lastlactate.dftime[1])))
# names(nocleardata) <- c('itemid', 'dftime', 'status', 'cleartime')
# for (i in 2:nrow(noclear.pts)) {
#   nocleardata0 <- c(noclear.pts$itemid[i], noclear.pts$dftime[i], 1, noclear.pts$lastlactate.dftime[i])
#   nocleardata <- rbind(nocleardata, nocleardata0)
# }
# 
# 
# censordata <- data.frame(t(c(censored.pts$itemid[1], censored.pts$dftime[1], 0, censored.pts$lastlactate.dftime[1])))
# if (censored.pts$lastlactate.dftime[1] > censored.pts$dftime[1]) {
#   censordata <- data.frame(t(c(censored.pts$itemid[1], censored.pts$dftime[1], 0, censored.pts$lastlactate.dftime[1])))
# } else {
#   censordata <- data.frame(t(c(censored.pts$itemid[1], censored.pts$dftime[1], 0, censored.pts$dftime[1])))
# }
# 
# names(censordata) <- c('itemid', 'dftime', 'status', 'cleartime')
# for (i in 2:nrow(censored.pts)) {
#   if (censored.pts$lastlactate.dftime[i] > censored.pts$dftime[i]) {
#     censordata0 <- c(censored.pts$itemid[i], censored.pts$dftime[i], 0, censored.pts$lastlactate.dftime[i])
#   } else {
#     censordata0 <- c(censored.pts$itemid[i], censored.pts$dftime[i], 0, censored.pts$dftime[i])
#   }
#   censordata <- rbind(censordata, censordata0)
# }
# 
# 
# modeldata <- rbind(cleardata, nocleardata, censordata)
# modeldata$start <- 0
# modeldata$itemid <- as.factor(modeldata$itemid)
# modeldata$status <- as.factor(modeldata$status)
# summary(coxph(Surv(start, cleartime, status), modeldata)) 
# # summary(coxph(Surv(start, cleartime, status) ~ (itemid + dftime), modeldata)) 
# summary(modeldata)
# 
# modeldata0 <- modeldata[modeldata$cleartime ==0,]




load('pt32788.RData')
load('pt11923.RData')
load('pt17766.RData')
load('pt25946.RData')
load('pt30425.RData')
load('pt5962.RData')
load('pt10004.RData')
load('pt15918.RData')
load('pt10019.RData')
load('pt12740.RData')
load('pt25498.RData')
load('pt12292.RData')
load('pt13575.RData')





